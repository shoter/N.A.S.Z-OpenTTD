/* $Id: afterload.cpp 27320 2015-06-27 11:46:41Z frosch $ */

/*
 * This file is part of OpenTTD.
 * OpenTTD is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, version 2.
 * OpenTTD is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with OpenTTD. If not, see <http://www.gnu.org/licenses/>.
 */

/** @file afterload.cpp Code updating data after game load */

#include "../stdafx.h"
#include "../void_map.h"
#include "../signs_base.h"
#include "../depot_base.h"
#include "../fios.h"
#include "../gamelog_internal.h"
#include "../network/network.h"
#include "../network/network_func.h"
#include "../gfxinit.h"
#include "../viewport_func.h"
#include "../industry.h"
#include "../clear_map.h"
#include "../vehicle_func.h"
#include "../string_func.h"
#include "../date_func.h"
#include "../roadveh.h"
#include "../train.h"
#include "../station_base.h"
#include "../waypoint_base.h"
#include "../roadstop_base.h"
#include "../tunnelbridge_map.h"
#include "../pathfinder/yapf/yapf_cache.h"
#include "../elrail_func.h"
#include "../signs_func.h"
#include "../aircraft.h"
#include "../object_map.h"
#include "../object_base.h"
#include "../tree_map.h"
#include "../company_func.h"
#include "../road_cmd.h"
#include "../ai/ai.hpp"
#include "../ai/ai_gui.hpp"
#include "../town.h"
#include "../economy_base.h"
#include "../animated_tile_func.h"
#include "../subsidy_base.h"
#include "../subsidy_func.h"
#include "../newgrf.h"
#include "../engine_func.h"
#include "../rail_gui.h"
#include "../core/backup_type.hpp"
#include "../smallmap_gui.h"
#include "../news_func.h"
#include "../order_backup.h"
#include "../error.h"
#include "../disaster_vehicle.h"


#include "saveload_internal.h"

#include <signal.h>

#include "../safeguards.h"

extern Company *DoStartupNewCompany(bool is_ai, CompanyID company = INVALID_COMPANY);

/**
 * Makes a tile canal or water depending on the surroundings.
 *
 * Must only be used for converting old savegames. Use WaterClass now.
 *
 * This as for example docks and shipdepots do not store
 * whether the tile used to be canal or 'normal' water.
 * @param t the tile to change.
 * @param include_invalid_water_class Also consider WATER_CLASS_INVALID, i.e. industry tiles on land
 */
void SetWaterClassDependingOnSurroundings(TileIndex t, bool include_invalid_water_class)
{
	/* If the slope is not flat, we always assume 'land' (if allowed). Also for one-corner-raised-shores.
	 * Note: Wrt. autosloping under industry tiles this is the most fool-proof behaviour. */
	if (!IsTileFlat(t)) {
		if (include_invalid_water_class) {
			SetWaterClass(t, WATER_CLASS_INVALID);
			return;
		} else {
			SlErrorCorrupt("Invalid water class for dry tile");
		}
	}

	/* Mark tile dirty in all cases */
	MarkTileDirtyByTile(t);

	if (TileX(t) == 0 || TileY(t) == 0 || TileX(t) == MapMaxX() - 1 || TileY(t) == MapMaxY() - 1) {
		/* tiles at map borders are always WATER_CLASS_SEA */
		SetWaterClass(t, WATER_CLASS_SEA);
		return;
	}

	bool has_water = false;
	bool has_canal = false;
	bool has_river = false;

	for (DiagDirection dir = DIAGDIR_BEGIN; dir < DIAGDIR_END; dir++) {
		TileIndex neighbour = TileAddByDiagDir(t, dir);
		switch (GetTileType(neighbour)) {
			case MP_WATER:
				/* clear water and shipdepots have already a WaterClass associated */
				if (IsCoast(neighbour)) {
					has_water = true;
				} else if (!IsLock(neighbour)) {
					switch (GetWaterClass(neighbour)) {
						case WATER_CLASS_SEA:   has_water = true; break;
						case WATER_CLASS_CANAL: has_canal = true; break;
						case WATER_CLASS_RIVER: has_river = true; break;
						default: SlErrorCorrupt("Invalid water class for tile");
					}
				}
				break;

			case MP_RAILWAY:
				/* Shore or flooded halftile */
				has_water |= (GetRailGroundType(neighbour) == RAIL_GROUND_WATER);
				break;

			case MP_TREES:
				/* trees on shore */
				has_water |= (GB(_m[neighbour].m2, 4, 2) == TREE_GROUND_SHORE);
				break;

			default: break;
		}
	}

	if (!has_water && !has_canal && !has_river && include_invalid_water_class) {
		SetWaterClass(t, WATER_CLASS_INVALID);
		return;
	}

	if (has_river && !has_canal) {
		SetWaterClass(t, WATER_CLASS_RIVER);
	} else if (has_canal || !has_water) {
		SetWaterClass(t, WATER_CLASS_CANAL);
	} else {
		SetWaterClass(t, WATER_CLASS_SEA);
	}
}

static void ConvertTownOwner()
{
	for (TileIndex tile = 0; tile != MapSize(); tile++) {
		switch (GetTileType(tile)) {
			case MP_ROAD:
				if (GB(_m[tile].m5, 4, 2) == ROAD_TILE_CROSSING && HasBit(_m[tile].m3, 7)) {
					_m[tile].m3 = OWNER_TOWN;
				}
				/* FALL THROUGH */

			case MP_TUNNELBRIDGE:
				if (_m[tile].m1 & 0x80) SetTileOwner(tile, OWNER_TOWN);
				break;

			default: break;
		}
	}
}

/* since savegame version 4.1, exclusive transport rights are stored at towns */
static void UpdateExclusiveRights()
{
	Town *t;

	FOR_ALL_TOWNS(t) {
		t->exclusivity = INVALID_COMPANY;
	}

	/* FIXME old exclusive rights status is not being imported (stored in s->blocked_months_obsolete)
	 *   could be implemented this way:
	 * 1.) Go through all stations
	 *     Build an array town_blocked[ town_id ][ company_id ]
	 *     that stores if at least one station in that town is blocked for a company
	 * 2.) Go through that array, if you find a town that is not blocked for
	 *     one company, but for all others, then give him exclusivity.
	 */
}

static const byte convert_currency[] = {
	 0,  1, 12,  8,  3,
	10, 14, 19,  4,  5,
	 9, 11, 13,  6, 17,
	16, 22, 21,  7, 15,
	18,  2, 20,
};

/* since savegame version 4.2 the currencies are arranged differently */
static void UpdateCurrencies()
{
	_settings_game.locale.currency = convert_currency[_settings_game.locale.currency];
}

/* Up to revision 1413 the invisible tiles at the southern border have not been
 * MP_VOID, even though they should have. This is fixed by this function
 */
static void UpdateVoidTiles()
{
	uint i;

	for (i = 0; i < MapMaxY(); ++i) MakeVoid(i * MapSizeX() + MapMaxX());
	for (i = 0; i < MapSizeX(); ++i) MakeVoid(MapSizeX() * MapMaxY() + i);
}

static inline RailType UpdateRailType(RailType rt, RailType min)
{
	return rt >= min ? (RailType)(rt + 1): rt;
}

/**
 * Update the viewport coordinates of all signs.
 */
void UpdateAllVirtCoords()
{
	UpdateAllStationVirtCoords();
	UpdateAllSignVirtCoords();
	UpdateAllTownVirtCoords();
}

/**
 * Initialization of the windows and several kinds of caches.
 * This is not done directly in AfterLoadGame because these
 * functions require that all saveload conversions have been
 * done. As people tend to add savegame conversion stuff after
 * the intialization of the windows and caches quite some bugs
 * had been made.
 * Moving this out of there is both cleaner and less bug-prone.
 */
static void InitializeWindowsAndCaches()
{
	/* Initialize windows */
	ResetWindowSystem();
	SetupColoursAndInitialWindow();

	/* Update coordinates of the signs. */
	UpdateAllVirtCoords();
	ResetViewportAfterLoadGame();

	Company *c;
	FOR_ALL_COMPANIES(c) {
		/* For each company, verify (while loading a scenario) that the inauguration date is the current year and set it
		 * accordingly if it is not the case.  No need to set it on companies that are not been used already,
		 * thus the MIN_YEAR (which is really nothing more than Zero, initialized value) test */
		if (_file_to_saveload.filetype == FT_SCENARIO && c->inaugurated_year != MIN_YEAR) {
			c->inaugurated_year = _cur_year;
		}
	}

	/* Count number of objects per type */
	Object *o;
	FOR_ALL_OBJECTS(o) {
		Object::IncTypeCount(o->type);
	}

	/* Identify owners of persistent storage arrays */
	Industry *i;
	FOR_ALL_INDUSTRIES(i) {
		if (i->psa != NULL) {
			i->psa->feature = GSF_INDUSTRIES;
			i->psa->tile = i->location.tile;
		}
	}
	Station *s;
	FOR_ALL_STATIONS(s) {
		if (s->airport.psa != NULL) {
			s->airport.psa->feature = GSF_AIRPORTS;
			s->airport.psa->tile = s->airport.tile;
		}
	}
	Town *t;
	FOR_ALL_TOWNS(t) {
		for (std::list<PersistentStorage *>::iterator it = t->psa_list.begin(); it != t->psa_list.end(); ++it) {
			(*it)->feature = GSF_FAKE_TOWNS;
			(*it)->tile = t->xy;
		}
	}

	RecomputePrices();

	GroupStatistics::UpdateAfterLoad();

	Station::RecomputeIndustriesNearForAll();
	RebuildSubsidisedSourceAndDestinationCache();

	/* Towns have a noise controlled number of airports system
	 * So each airport's noise value must be added to the town->noise_reached value
	 * Reset each town's noise_reached value to '0' before. */
	UpdateAirportsNoise();

	CheckTrainsLengths();
	ShowNewGRFError();
	ShowAIDebugWindowIfAIError();

	/* Rebuild the smallmap list of owners. */
	BuildOwnerLegend();
}

typedef void (CDECL *SignalHandlerPointer)(int);
static SignalHandlerPointer _prev_segfault = NULL;
static SignalHandlerPointer _prev_abort    = NULL;
static SignalHandlerPointer _prev_fpe      = NULL;

static void CDECL HandleSavegameLoadCrash(int signum);

/**
 * Replaces signal handlers of SIGSEGV and SIGABRT
 * and stores pointers to original handlers in memory.
 */
static void SetSignalHandlers()
{
	_prev_segfault = signal(SIGSEGV, HandleSavegameLoadCrash);
	_prev_abort    = signal(SIGABRT, HandleSavegameLoadCrash);
	_prev_fpe      = signal(SIGFPE,  HandleSavegameLoadCrash);
}

/**
 * Resets signal handlers back to original handlers.
 */
static void ResetSignalHandlers()
{
	signal(SIGSEGV, _prev_segfault);
	signal(SIGABRT, _prev_abort);
	signal(SIGFPE,  _prev_fpe);
}

/**
 * Try to find the overridden GRF identifier of the given GRF.
 * @param c the GRF to get the 'previous' version of.
 * @return the GRF identifier or \a c if none could be found.
 */
static const GRFIdentifier *GetOverriddenIdentifier(const GRFConfig *c)
{
	const LoggedAction *la = &_gamelog_action[_gamelog_actions - 1];
	if (la->at != GLAT_LOAD) return &c->ident;

	const LoggedChange *lcend = &la->change[la->changes];
	for (const LoggedChange *lc = la->change; lc != lcend; lc++) {
		if (lc->ct == GLCT_GRFCOMPAT && lc->grfcompat.grfid == c->ident.grfid) return &lc->grfcompat;
	}

	return &c->ident;
}

/** Was the saveload crash because of missing NewGRFs? */
static bool _saveload_crash_with_missing_newgrfs = false;

/**
 * Did loading the savegame cause a crash? If so,
 * were NewGRFs missing?
 * @return when the saveload crashed due to missing NewGRFs.
 */
bool SaveloadCrashWithMissingNewGRFs()
{
	return _saveload_crash_with_missing_newgrfs;
}

/**
 * Signal handler used to give a user a more useful report for crashes during
 * the savegame loading process; especially when there's problems with the
 * NewGRFs that are required by the savegame.
 * @param signum received signal
 */
static void CDECL HandleSavegameLoadCrash(int signum)
{
	ResetSignalHandlers();

	char buffer[8192];
	char *p = buffer;
	p += seprintf(p, lastof(buffer), "Loading your savegame caused OpenTTD to crash.\n");

	for (const GRFConfig *c = _grfconfig; !_saveload_crash_with_missing_newgrfs && c != NULL; c = c->next) {
		_saveload_crash_with_missing_newgrfs = HasBit(c->flags, GCF_COMPATIBLE) || c->status == GCS_NOT_FOUND;
	}

	if (_saveload_crash_with_missing_newgrfs) {
		p += seprintf(p, lastof(buffer),
			"This is most likely caused by a missing NewGRF or a NewGRF that\n"
			"has been loaded as replacement for a missing NewGRF. OpenTTD\n"
			"cannot easily determine whether a replacement NewGRF is of a newer\n"
			"or older version.\n"
			"It will load a NewGRF with the same GRF ID as the missing NewGRF.\n"
			"This means that if the author makes incompatible NewGRFs with the\n"
			"same GRF ID OpenTTD cannot magically do the right thing. In most\n"
			"cases OpenTTD will load the savegame and not crash, but this is an\n"
			"exception.\n"
			"Please load the savegame with the appropriate NewGRFs installed.\n"
			"The missing/compatible NewGRFs are:\n");

		for (const GRFConfig *c = _grfconfig; c != NULL; c = c->next) {
			if (HasBit(c->flags, GCF_COMPATIBLE)) {
				const GRFIdentifier *replaced = GetOverriddenIdentifier(c);
				char buf[40];
				md5sumToString(buf, lastof(buf), replaced->md5sum);
				p += seprintf(p, lastof(buffer), "NewGRF %08X (checksum %s) not found.\n  Loaded NewGRF \"%s\" with same GRF ID instead.\n", BSWAP32(c->ident.grfid), buf, c->filename);
			}
			if (c->status == GCS_NOT_FOUND) {
				char buf[40];
				md5sumToString(buf, lastof(buf), c->ident.md5sum);
				p += seprintf(p, lastof(buffer), "NewGRF %08X (%s) not found; checksum %s.\n", BSWAP32(c->ident.grfid), c->filename, buf);
			}
		}
	} else {
		p += seprintf(p, lastof(buffer),
			"This is probably caused by a corruption in the savegame.\n"
			"Please file a bug report and attach this savegame.\n");
	}

	ShowInfo(buffer);

	SignalHandlerPointer call = NULL;
	switch (signum) {
		case SIGSEGV: call = _prev_segfault; break;
		case SIGABRT: call = _prev_abort; break;
		case SIGFPE:  call = _prev_fpe; break;
		default: NOT_REACHED();
	}
	if (call != NULL) call(signum);
}



/**
 * Checks for the possibility that a bridge may be on this tile
 * These are in fact all the tile types on which a bridge can be found
 * @param t The tile to analyze
 * @return True if a bridge might have been present prior to savegame 194.
 */
static inline bool MayHaveBridgeAbove(TileIndex t)
{
	return IsTileType(t, MP_CLEAR) || IsTileType(t, MP_RAILWAY) || IsTileType(t, MP_ROAD) ||
			IsTileType(t, MP_WATER) || IsTileType(t, MP_TUNNELBRIDGE) || IsTileType(t, MP_OBJECT);
}

/**
 * Perform a (large) amount of savegame conversion *magic* in order to
 * load older savegames and to fill the caches for various purposes.
 * @return True iff conversion went without a problem.
 */
bool AfterLoadGame()
{
	SetSignalHandlers();

	TileIndex map_size = MapSize();

	extern TileIndex _cur_tileloop_tile; // From landscape.cpp.
	/* The LFSR used in RunTileLoop iteration cannot have a zeroed state, make it non-zeroed. */
	if (_cur_tileloop_tile == 0) _cur_tileloop_tile = 1;


	GamelogTestRevision();
	GamelogTestMode();


	if (IsSavegameVersionBefore(119)) {
		_pause_mode = (_pause_mode == 2) ? PM_PAUSED_NORMAL : PM_UNPAUSED;
	} else if (_network_dedicated && (_pause_mode & PM_PAUSED_ERROR) != 0) {
		DEBUG(net, 0, "The loading savegame was paused due to an error state.");
		DEBUG(net, 0, "  The savegame cannot be used for multiplayer!");
		/* Restore the signals */
		ResetSignalHandlers();
		return false;
	} else if (!_networking || _network_server) {
		/* If we are in single player, i.e. not networking, and loading the
		 * savegame or we are loading the savegame as network server we do
		 * not want to be bothered by being paused because of the automatic
		 * reason of a network server, e.g. joining clients or too few
		 * active clients. Note that resetting these values for a network
		 * client are very bad because then the client is going to execute
		 * the game loop when the server is not, i.e. it desyncs. */
		_pause_mode &= ~PMB_PAUSED_NETWORK;
	}







	/* From this point the old names array is cleared. */
	ResetOldNames();



	/* convert road side to my format. */
	if (_settings_game.vehicle.road_side) _settings_game.vehicle.road_side = 1;

	/* Check if all NewGRFs are present, we are very strict in MP mode */
	GRFListCompatibility gcf_res = IsGoodGRFConfigList(_grfconfig);
	for (GRFConfig *c = _grfconfig; c != NULL; c = c->next) {
		if (c->status == GCS_NOT_FOUND) {
			GamelogGRFRemove(c->ident.grfid);
		} else if (HasBit(c->flags, GCF_COMPATIBLE)) {
			GamelogGRFCompatible(&c->ident);
		}
	}

	if (_networking && gcf_res != GLC_ALL_GOOD) {
		SetSaveLoadError(STR_NETWORK_ERROR_CLIENT_NEWGRF_MISMATCH);
		/* Restore the signals */
		ResetSignalHandlers();
		return false;
	}

	switch (gcf_res) {
		case GLC_COMPATIBLE: ShowErrorMessage(STR_NEWGRF_COMPATIBLE_LOAD_WARNING, INVALID_STRING_ID, WL_CRITICAL); break;
		case GLC_NOT_FOUND:  ShowErrorMessage(STR_NEWGRF_DISABLED_WARNING, INVALID_STRING_ID, WL_CRITICAL); _pause_mode = PM_PAUSED_ERROR; break;
		default: break;
	}

	/* Update current year
	 * must be done before loading sprites as some newgrfs check it */
	SetDate(_date, _date_fract);

	/* Load the sprites */
	GfxLoadSprites();
	LoadStringWidthTable();

	/* Copy temporary data to Engine pool */
	CopyTempEngineData();

	/* Connect front and rear engines of multiheaded trains and converts
	 * subtype to the new format */
	if (IsSavegameVersionBefore(17, 1)) ConvertOldMultiheadToNew();

	/* Connect front and rear engines of multiheaded trains */
	ConnectMultiheadedTrains();

	/* Fix the CargoPackets *and* fix the caches of CargoLists.
	 * If this isn't done before Stations and especially Vehicles are
	 * running their AfterLoad we might get in trouble. In the case of
	 * vehicles we could give the wrong (cached) count of items in a
	 * vehicle which causes different results when getting their caches
	 * filled; and that could eventually lead to desyncs. */
	CargoPacket::AfterLoad();


	/* Update all vehicles */
	AfterLoadVehicles(true);

	/* Make sure there is an AI attached to an AI company */
	{
		Company *c;
		FOR_ALL_COMPANIES(c) {
			if (c->is_ai && c->ai_instance == NULL) AI::StartNew(c->index);
		}
	}

	/* make sure there is a town in the game */
	if (_game_mode == GM_NORMAL && Town::GetNumItems() == 0) {
		SetSaveLoadError(STR_ERROR_NO_TOWN_IN_SCENARIO);
		/* Restore the signals */
		ResetSignalHandlers();
		return false;
	}

	/* The void tiles on the southern border used to belong to a wrong class (pre 4.3).
	 * This problem appears in savegame version 21 too, see r3455. But after loading the
	 * savegame and saving again, the buggy map array could be converted to new savegame
	 * version. It didn't show up before r12070. */
	if (IsSavegameVersionBefore(87)) UpdateVoidTiles();

	/* If Load Scenario / New (Scenario) Game is used,
	 *  a company does not exist yet. So create one here.
	 * 1 exception: network-games. Those can have 0 companies
	 *   But this exception is not true for non-dedicated network servers! */
	if (!Company::IsValidID(COMPANY_FIRST) && (!_networking || (_networking && _network_server && !_network_dedicated))) {
		DoStartupNewCompany(false);
		Company *c = Company::Get(COMPANY_FIRST);
		c->settings = _settings_client.company;
	}

	/* Fix the cache for cargo payments. */
	CargoPayment *cp;
	FOR_ALL_CARGO_PAYMENTS(cp) {
		cp->front->cargo_payment = cp;
		cp->current_station = cp->front->last_station_visited;
	}

	for (TileIndex t = 0; t < map_size; t++) {
		switch (GetTileType(t)) {
			case MP_STATION: {
				BaseStation *bst = BaseStation::GetByTile(t);

				/* Set up station spread */
				bst->rect.BeforeAddTile(t, StationRect::ADD_FORCE);

				/* Waypoints don't have road stops/oil rigs in the old format */
				if (!Station::IsExpected(bst)) break;
				Station *st = Station::From(bst);

				switch (GetStationType(t)) {
					case STATION_TRUCK:
					case STATION_BUS:
						if (IsSavegameVersionBefore(6)) {
							/* Before version 5 you could not have more than 250 stations.
							 * Version 6 adds large maps, so you could only place 253*253
							 * road stops on a map (no freeform edges) = 64009. So, yes
							 * someone could in theory create such a full map to trigger
							 * this assertion, it's safe to assume that's only something
							 * theoretical and does not happen in normal games. */
							assert(RoadStop::CanAllocateItem());

							/* From this version on there can be multiple road stops of the
							 * same type per station. Convert the existing stops to the new
							 * internal data structure. */
							RoadStop *rs = new RoadStop(t);

							RoadStop **head =
								IsTruckStop(t) ? &st->truck_stops : &st->bus_stops;
							*head = rs;
						}
						break;

					case STATION_OILRIG: {
						/* Very old savegames sometimes have phantom oil rigs, i.e.
						 * an oil rig which got shut down, but not completely removed from
						 * the map
						 */
						TileIndex t1 = TILE_ADDXY(t, 0, 1);
						if (IsTileType(t1, MP_INDUSTRY) &&
								GetIndustryGfx(t1) == GFX_OILRIG_1) {
							/* The internal encoding of oil rigs was changed twice.
							 * It was 3 (till 2.2) and later 5 (till 5.1).
							 * Setting it unconditionally does not hurt.
							 */
							Station::GetByTile(t)->airport.type = AT_OILRIG;
						} else {
							DeleteOilRig(t);
						}
						break;
					}

					default: break;
				}
				break;
			}

			default: break;
		}
	

	YapfNotifyTrackLayoutChange(INVALID_TILE, INVALID_TRACK);


	Company *c;
	FOR_ALL_COMPANIES(c) {
		c->avail_railtypes = GetCompanyRailtypes(c->index);
		c->avail_roadtypes = GetCompanyRoadtypes(c->index);
	}

	if (!IsSavegameVersionBefore(27)) AfterLoadStations();




	/* do the same as when elrails were enabled/disabled manually just now */
	SettingsDisableElrail(_settings_game.vehicle.disable_elrails);
	InitializeRailGUI();


	/* Check and update house and town values */
	UpdateHousesAndTowns();


		/* For some reason non-loading vehicles could be in the station's loading vehicle list */

		Station *st;
		FOR_ALL_STATIONS(st) {
			std::list<Vehicle *>::iterator iter;
			for (iter = st->loading_vehicles.begin(); iter != st->loading_vehicles.end();) {
				Vehicle *v = *iter;
				iter++;
				if (!v->current_order.IsType(OT_LOADING)) st->loading_vehicles.remove(v);
			}
		}
	}





	/* When any NewGRF has been changed the availability of some vehicles might
	 * have been changed too. e->company_avail must be set to 0 in that case
	 * which is done by StartupEngines(). */
	if (gcf_res != GLC_ALL_GOOD) StartupEngines();


	/*
	 * Only keep order-backups for network clients (and when replaying).
	 * If we are a network server or not networking, then we just loaded a previously
	 * saved-by-server savegame. There are no clients with a backup, so clear it.
	 * Furthermore before savegame version 192 the actual content was always corrupt.
	 */
	if (!_networking || _network_server || IsSavegameVersionBefore(192)) {
#ifndef DEBUG_DUMP_COMMANDS
		/* Note: We cannot use CleanPool since that skips part of the destructor
		 * and then leaks un-reachable Orders in the order pool. */
		OrderBackup *ob;
		FOR_ALL_ORDER_BACKUPS(ob) {
			delete ob;
		}
#endif
	}

	/* Road stops is 'only' updating some caches */
	AfterLoadRoadStops();
	AfterLoadLabelMaps();
	AfterLoadCompanyStats();
	AfterLoadStoryBook();

	GamelogPrintDebug(1);

	InitializeWindowsAndCaches();
	/* Restore the signals */
	ResetSignalHandlers();

	AfterLoadLinkGraphs();
	return true;
}

/**
 * Reload all NewGRF files during a running game. This is a cut-down
 * version of AfterLoadGame().
 * XXX - We need to reset the vehicle position hash because with a non-empty
 * hash AfterLoadVehicles() will loop infinitely. We need AfterLoadVehicles()
 * to recalculate vehicle data as some NewGRF vehicle sets could have been
 * removed or added and changed statistics
 */
void ReloadNewGRFData()
{
	/* reload grf data */
	GfxLoadSprites();
	LoadStringWidthTable();
	RecomputePrices();
	/* reload vehicles */
	ResetVehicleHash();
	AfterLoadVehicles(false);
	StartupEngines();
	GroupStatistics::UpdateAfterLoad();
	/* update station graphics */
	AfterLoadStations();
	/* Update company statistics. */
	AfterLoadCompanyStats();
	/* Check and update house and town values */
	UpdateHousesAndTowns();
	/* Delete news referring to no longer existing entities */
	DeleteInvalidEngineNews();
	/* Update livery selection windows */
	for (CompanyID i = COMPANY_FIRST; i < MAX_COMPANIES; i++) InvalidateWindowData(WC_COMPANY_COLOUR, i);
	/* Update company infrastructure counts. */
	InvalidateWindowClassesData(WC_COMPANY_INFRASTRUCTURE);
	/* redraw the whole screen */
	MarkWholeScreenDirty();
	CheckTrainsLengths();
}
