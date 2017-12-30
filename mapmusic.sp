#pragma semicolon 1

#include <sdktools>
#include <sdkhooks>
#include <dhooks>
#include <regex>

#pragma newdecls required

Handle hAcceptInput;
Regex regPattern;
RegexError regError;

bool g_bDisabled[MAXPLAYERS+1] = false;
float g_fVolume[MAXPLAYERS+1] = {1.0, ...};
int randomChannel;
StringMap g_smSourceEnts;
StringMap g_smChannel;
StringMap g_smCommon;
StringMap g_smRecent;
StringMap g_smVolume;

public Plugin myinfo =
{
    name        = "Map Music Controller",
    author      = "Mitch & Agent Wesker & Kyle",
    description = "",
    version     = "1.0",
    url         = "https://ump45.moe"
};

public APLRes AskPluginLoad2(Handle myself, bool late, char[] error, int err_max)
{
    CreateNative("MapMusic_GetVolume", Native_GetVolume);
    CreateNative("MapMusic_SetVolume", Native_SetVolume);
    CreateNative("MapMusic_GetStatus", Native_GetStatus);
    CreateNative("MapMusic_SetStatus", Native_SetStatus);

    return APLRes_Success;
}

public int Native_GetVolume(Handle myself, int numParams)
{
    return RoundToCeil(g_fVolume[GetNativeCell(1)] * 100);
}

public int Native_SetVolume(Handle myself, int numParams)
{
    int client = GetNativeCell(1);
    int volume = GetNativeCell(2);
    
    g_fVolume[client] = volume * 0.01;

    if(volume <= 0)
    {
        g_fVolume[client] = 0.0;
    }
    
    if(volume > 100)
    {
        g_fVolume[client] = 1.0;
    }

    if(IsClientInGame(client))
    {
        PrintToChat(client, "[\x04MapMusic\x01]  \x05Volume\x01:  %d", RoundToCeil(g_fVolume[client]*100));
        if(g_bDisabled[client] || g_fVolume[client] <= 0.0)
            ClientStopSound(client, "", false);
    }
}

public int Native_GetStatus(Handle myself, int numParams)
{
    return g_bDisabled[GetNativeCell(1)];
}

public int Native_SetStatus(Handle myself, int numParams)
{
    int client = GetNativeCell(1);
    g_bDisabled[client] = GetNativeCell(2);
    if(IsClientInGame(client))
    {
        PrintToChat(GetNativeCell(1), "[\x04MapMusic\x01]  \x05BGM\x01:  %s", GetNativeCell(2) ? "Off" : "On");
        if(g_bDisabled[client] || g_fVolume[client] <= 0.0)
            ClientStopSound(client, "", false);
    }
}

public void OnPluginStart()
{
    if(g_smSourceEnts == null)
        g_smSourceEnts = new StringMap();

    if(g_smChannel == null)
        g_smChannel = new StringMap();

    if(g_smCommon == null)
        g_smCommon = new StringMap();

    if(g_smRecent == null)
        g_smRecent = new StringMap();

    if(g_smVolume == null)
        g_smVolume = new StringMap();

    char preError[256];
    char prePattern[256] = "(([-_a-zA-Z0-9]+[/]?)+[.][a-zA-Z0-9]{3})";
    regPattern = CompileRegex(prePattern, PCRE_CASELESS, preError, 256, regError);
    if(regError != REGEX_ERROR_NONE)
        LogError(preError);

    if(hAcceptInput == null)
    {
        char tmpOffset[148];

        switch(GetEngineVersion())
        {
            case Engine_CSGO:
                tmpOffset = "sdktools.games\\engine.csgo";
            case Engine_CSS:
                tmpOffset = "sdktools.games\\engine.css";
            case Engine_TF2:
                tmpOffset = "sdktools.games\\engine.tf";
            case Engine_Contagion:
                tmpOffset = "sdktools.games\\engine.contagion";
            case Engine_Left4Dead2:
                tmpOffset = "sdktools.games\\engine.Left4Dead2";
            case Engine_AlienSwarm:
                tmpOffset = "sdktools.games\\engine.swarm";
        }

        Handle temp = LoadGameConfigFile(tmpOffset);
    
        if(temp == null)
            SetFailState("Why you no has gamedata?");

        HookEvent("round_poststart", Event_PostRoundStart);
        
        int offset = GameConfGetOffset(temp, "AcceptInput");
        hAcceptInput = DHookCreate(offset, HookType_Entity, ReturnType_Bool, ThisPointer_CBaseEntity, AcceptInput);
        DHookAddParam(hAcceptInput, HookParamType_CharPtr);
        DHookAddParam(hAcceptInput, HookParamType_CBaseEntity);
        DHookAddParam(hAcceptInput, HookParamType_CBaseEntity);
        DHookAddParam(hAcceptInput, HookParamType_Object, 20);
        DHookAddParam(hAcceptInput, HookParamType_Int);

        delete temp;
    }
    
    RegConsoleCmd("sm_mapmusic", Command);
    
    RegPluginLibrary("MapMusic");
}

public Action Command(int client, int args)
{
    PrintToChat(client, "[\x04MapMusic\x01]  \x05BGM\x01: %s  \x05Volume\x01: %d", g_bDisabled[client] ? "Off" : "On", RoundToCeil(g_fVolume[client]*100));
    FakeClientCommand(client, "sm_music");
    return Plugin_Handled;
}

public void OnClientConnected(int client)
{
    g_bDisabled[client] = false;
    g_fVolume[client] = 1.0;
}

public void OnMapStart()
{
    g_smSourceEnts.Clear();
    g_smChannel.Clear();
    g_smCommon.Clear();
    g_smRecent.Clear();
    g_smVolume.Clear();
    randomChannel = SNDCHAN_USER_BASE - 75;
}

public void Event_PostRoundStart(Event event, const char[] name, bool dontBroadcast)
{
    g_smRecent.Clear();
    g_smVolume.Clear();
    for(int j = 1; j <= MaxClients; j++)
        if(IsValidClient(j))
        {
            ClientCommand(j, "snd_setsoundparam Music.StartRound.valve_csgo_01 volume 0");
            ClientCommand(j, "snd_setsoundparam Music.StartRound_01.valve_csgo_01 volume 0");
            ClientCommand(j, "snd_setsoundparam Music.StartRound_02.valve_csgo_01 volume 0");
            ClientCommand(j, "snd_setsoundparam Music.StartRound_03.valve_csgo_01 volume 0");
            ClientCommand(j, "snd_setsoundparam Music.StartAction.valve_csgo_01 volume 0");
            ClientCommand(j, "snd_setsoundparam Music.StartAction_01.valve_csgo_01 volume 0"); 
            ClientCommand(j, "snd_setsoundparam Music.DeathCam.valve_csgo_01 volume 0");
            ClientCommand(j, "snd_setsoundparam Music.LostRound.valve_csgo_01 volume 0");
            ClientCommand(j, "snd_setsoundparam Music.WonRound.valve_csgo_01 volume 0");
            ClientCommand(j, "snd_setsoundparam Music.MVPAnthem.valve_csgo_01 volume 0");
            ClientCommand(j, "snd_setsoundparam Music.MVPAnthem_01.valve_csgo_01 volume 0");
            
            ClientCommand(j, "snd_setsoundparam Music.StartRound.valve_csgo_02 volume 0");
            ClientCommand(j, "snd_setsoundparam Music.StartRound_01.valve_csgo_02 volume 0");
            ClientCommand(j, "snd_setsoundparam Music.StartRound_02.valve_csgo_02 volume 0");
            ClientCommand(j, "snd_setsoundparam Music.StartRound_03.valve_csgo_02 volume 0");
            ClientCommand(j, "snd_setsoundparam Music.StartAction.valve_csgo_02 volume 0");
            ClientCommand(j, "snd_setsoundparam Music.StartAction_01.valve_csgo_02 volume 0"); 
            ClientCommand(j, "snd_setsoundparam Music.DeathCam.valve_csgo_02 volume 0");
            ClientCommand(j, "snd_setsoundparam Music.LostRound.valve_csgo_02 volume 0");
            ClientCommand(j, "snd_setsoundparam Music.WonRound.valve_csgo_02 volume 0");
            ClientCommand(j, "snd_setsoundparam Music.MVPAnthem.valve_csgo_02 volume 0");
            ClientCommand(j, "snd_setsoundparam Music.MVPAnthem_01.valve_csgo_02 volume 0");
        }
}

public MRESReturn AcceptInput(int entity, Handle hReturn, Handle hParams)
{
    //Abort if the entity is missing
    if(!IsValidEntity(entity))
        return MRES_Ignored;

    char eClassname[128], eCommand[128], eParam[128], soundFile[256];
    int eActivator;

    DHookGetParamString(hParams, 1, eCommand, 128);

    int type, iParam = -1;
    type = DHookGetParamObjectPtrVar(hParams, 4, 16, ObjectValueType_Int);

    if(type == 1)
        iParam = RoundFloat(DHookGetParamObjectPtrVar(hParams, 4, 0, ObjectValueType_Float));
    else if(type == 2)
    {
        DHookGetParamObjectPtrString(hParams, 4, 0, ObjectValueType_String, eParam, 128);
        StringToIntEx(eParam, iParam);
    }

    if(!DHookIsNullParam(hParams, 2))
    {
        eActivator = DHookGetParam(hParams, 2);
        if(eActivator < -1)
            eActivator = -1;
    }
    else
        eActivator = -1;

    GetEntityClassname(entity, eClassname, 128);

    if(strcmp(eClassname, "point_clientcommand", false) == 0)
    {
        //Don't allow client sounds to override this plugin
        if((StrContains(eParam, ".mp3", false) != -1) || (StrContains(eParam, ".wav", false) != -1))
        {
            int matchCount = MatchRegex(regPattern, eParam, regError);
            if(matchCount > 0)
            {
                if(GetRegexSubString(regPattern, 0, soundFile, 256))
                {
                    AddToStringTable(FindStringTable("soundprecache"), FakePrecacheSound(soundFile, true));
                    PrecacheSound(FakePrecacheSound(soundFile, true), false);
                    ClientSendSound(soundFile, eActivator, true);
                }
            }
            DHookSetReturn(hReturn, false);
            return MRES_Supercede;
        }
        return MRES_Ignored;
    }
    
    GetEntPropString(entity, Prop_Data, "m_iszSound", soundFile, 256);
    int eFlags = GetEntProp(entity, Prop_Data, "m_spawnflags");

    if(StrEqual(eCommand, "PlaySound", false) || StrEqual(eCommand, "FadeIn", false) || (StrEqual(eCommand, "Volume", false) && (iParam > 0)) || StrEqual(eCommand, "ToggleSound", false))
    {
        int temp;
        bool common = g_smCommon.GetValue(soundFile, temp);

        if(!((StrContains(soundFile, ".mp3", false) != -1) || (StrContains(soundFile, ".wav", false) != -1))) {
            //Workaround for client soundscripts (?)
            return MRES_Ignored;
        }
        
        if(eFlags & 1)
        {
            int curVol;
            if(g_smVolume.GetValue(soundFile, curVol) && (StrEqual(eCommand, "Volume", false) || StrEqual(eCommand, "ToggleSound", false)))
            {
                if((curVol != iParam) && StrEqual(eCommand, "Volume", false))
                {
                    //Different volume but already playing? Ignore
                    DHookSetReturn(hReturn, false);
                    return MRES_Supercede;
                } else if(StrEqual(eCommand, "ToggleSound", false)) {
                    //Sound was played already, so toggle the sound off
                    g_smVolume.Remove(soundFile);
                    StopSoundAll(soundFile, entity, common);
                    DHookSetReturn(hReturn, false);
                    return MRES_Supercede;
                }
            }
            else
            {
                if(StrEqual(eCommand, "PlaySound", false) || StrEqual(eCommand, "ToggleSound", false))
                    g_smVolume.SetValue(soundFile, 10, true);
                else if(StrEqual(eCommand, "Volume", false))
                    g_smVolume.SetValue(soundFile, iParam, true);
            }
        }

        if(g_smRecent.GetValue(soundFile, temp))
        {
            g_smRecent.Remove(soundFile);
            g_smCommon.SetValue(soundFile, 1, true);
            common = true;
            AddToStringTable(FindStringTable("soundprecache"), FakePrecacheSound(soundFile, true));
            PrecacheSound(FakePrecacheSound(soundFile, true), false);
        }
        else
        {
            AddToStringTable(FindStringTable("soundprecache"), FakePrecacheSound(soundFile, common));
            PrecacheSound(FakePrecacheSound(soundFile, common), false);
        }

        SendSoundAll(soundFile, entity, common);

        if(!common && !(eFlags & 1))
        {
            g_smRecent.SetValue(soundFile, 1, true);
            DataPack dataPack;
            CreateDataTimer(0.6, CheckCommonSounds, dataPack);
            dataPack.WriteString(soundFile);
            dataPack.WriteCell(entity);
        }
        DHookSetReturn(hReturn, false);
        return MRES_Supercede;
    } 
    else if(StrEqual(eCommand, "StopSound", false) || StrEqual(eCommand, "FadeOut", false) || (StrEqual(eCommand, "Volume", false) && (iParam == 0)))
    {
        int temp;
        bool common = g_smCommon.GetValue(soundFile, temp);    
        StopSoundAll(soundFile, entity, common);
        
        if(eFlags & 1)
        {
            g_smVolume.Remove(soundFile);
        }
        
        return MRES_Ignored;
    }
    
    return MRES_Ignored;
}

public int GetSourceEntity(int entity)
{
    char seName[64];
    GetEntPropString(entity, Prop_Data, "m_sSourceEntName", seName, sizeof(seName));
    if(seName[0])
    {
        int entRef;
        if(g_smSourceEnts.GetValue(seName, entRef))
        {
            int sourceEnt = EntRefToEntIndex(entRef);
            if(IsValidEntity(sourceEnt))
            {
                return sourceEnt;
            }
        }
    }
    return entity;
}

public Action CheckCommonSounds(Handle timer, DataPack dataPack)
{
    dataPack.Reset();
    char soundFile[256];
    dataPack.ReadString(soundFile, 256);
    g_smRecent.Remove(soundFile);
    int temp;
    if(g_smCommon.GetValue(soundFile, temp))
    {
        temp = dataPack.ReadCell();            
        StopSoundAll(soundFile, temp, false);
    }
}

public void OnEntityCreated(int entity, const char[] classname)
{
    if(!IsValidEdict(entity))
        return;
    
    if(StrEqual(classname, "ambient_generic", false))
    {
        DHookEntity(hAcceptInput, false, entity);
        SDKHook(entity, SDKHook_SpawnPost, OnEntitySpawned);
    }
    else if(StrEqual(classname, "point_clientcommand", false))
        DHookEntity(hAcceptInput, false, entity);
}

public void OnEntitySpawned(int entity)
{
    char seName[64], eName[64];
    GetEntPropString(entity, Prop_Data, "m_sSourceEntName", seName, 64);
    int eFlags = GetEntProp(entity, Prop_Data, "m_spawnflags");
    
    if(!(eFlags & 1) && seName[0])
        for (int i = 0; i <= GetEntityCount(); i++)
            if(IsValidEntity(i))
            {
                GetEntPropString(i, Prop_Data, "m_iName", eName, 64);
                if(StrEqual(seName, eName, false))
                {
                    g_smSourceEnts.SetValue(seName, EntIndexToEntRef(i), true);
                    return;
                }
            }
}

void SendSoundAll(char[] name, int entity, bool common = false)
{
    if(!IsValidEntity(entity))
        return;

    int eFlags = GetEntProp(entity, Prop_Data, "m_spawnflags");

    if(eFlags & 1)
    {
        int customChannel;
        
        if(!g_smChannel.GetValue(name, customChannel))
        {
            g_smChannel.SetValue(name, randomChannel, false);
            customChannel = randomChannel;
            randomChannel++;
            if(randomChannel > SNDCHAN_USER_BASE)
                randomChannel = SNDCHAN_USER_BASE - 75;
        }
        
        for (int i = 1; i <= MaxClients; i++)
            if(IsClientInGame(i) && !g_bDisabled[i] && g_fVolume[i] > 0.0)
                EmitSoundToClient(i, FakePrecacheSound(name, common), i, customChannel, SNDLEVEL_NORMAL, SND_NOFLAGS, g_fVolume[i], SNDPITCH_NORMAL, -1, _, _, true);
    }
    else
    {
        int sourceEnt = GetSourceEntity(entity);            
        for (int i = 1; i <= MaxClients; i++)
            if(IsClientInGame(i) && !g_bDisabled[i] && g_fVolume[i] > 0.0)
                EmitSoundToClient(i, FakePrecacheSound(name, common), sourceEnt, SNDCHAN_USER_BASE, SNDLEVEL_NORMAL, SND_NOFLAGS, g_fVolume[i], SNDPITCH_NORMAL, -1, _, _, true);
    }
}

void ClientSendSound(char[] name, int client, bool common = false)
{
    if(!IsValidClient(client))
        return;

    int customChannel;

    if(!g_smChannel.GetValue(name, customChannel))
    {
        g_smChannel.SetValue(name, randomChannel, false);
        customChannel = randomChannel;
        randomChannel++;
        if(randomChannel > SNDCHAN_USER_BASE)
        {
            randomChannel = SNDCHAN_USER_BASE - 75;
        }
    }

    if(!g_bDisabled[client] && g_fVolume[client] > 0.0)
        EmitSoundToClient(client, FakePrecacheSound(name, common), client, customChannel, SNDLEVEL_NORMAL, SND_NOFLAGS, g_fVolume[client], SNDPITCH_NORMAL, -1, _, _, true);
}

void ClientStopSound(int client, const char[] name = "", bool common = false)
{
    if(name[0])
    {
        int customChannel;
        if(g_smChannel.GetValue(name, customChannel))
            StopSound(client, customChannel, FakePrecacheSound(name, common));
        else
            StopSound(client, SNDCHAN_USER_BASE, FakePrecacheSound(name, common));
    }
    else
    {
        ClientCommand(client, "playgamesound Music.StopAllExceptMusic");
        ClientCommand(client, "playgamesound Music.StopAllMusic");
    }
}

void StopSoundAll(const char[] name, int entity, bool common = false)
{
    if(!IsValidEntity(entity))
        return;

    int eFlags = GetEntProp(entity, Prop_Data, "m_spawnflags");
    if(eFlags & 1)
    {
        for(int i = 1; i <= MaxClients; i++)
            if(IsClientInGame(i) && !g_bDisabled[i] && g_fVolume[i] > 0.0)
                ClientStopSound(i, name, common);
    }
    else
    {
        int sourceEnt = GetSourceEntity(entity);
        StopSound(sourceEnt, SNDCHAN_USER_BASE, FakePrecacheSound(name, common));
    }
}

stock static char[] FakePrecacheSound(const char[] sample, const bool common = false)
{
    char szSound[256];
    strcopy(szSound, 256, sample);
    if(common)
    {
        if(szSound[0] != '*')
        {
            if(szSound[0] == '#')
                Format(szSound, 256, "*%s", szSound[1]);
            else
                Format(szSound, 256, "*%s", szSound);
        }
    } else 
    {
        if(szSound[0] == '*')
            Format(szSound, 256, "%s", szSound[1]);
        if(szSound[0] == '#')
            Format(szSound, 256, "%s", szSound[1]);
    }
    return szSound;
}

bool IsValidClient(int client)
{
    return (1 <= client <= MAXPLAYERS && IsClientInGame(client));
} 