unit DAOCClasses;

(****************************************************************************
**
** Copyright (C) 2003 Bryan Mayland.  All rights reserved.
**
** This file may be distributed and/or modified under the terms of the
** GNU General Public License version 2 as published by the Free Software
** Foundation.
**
****************************************************************************)

interface

uses
  SysUtils;
  
type
  TDAOCCharacterClass = (ccUnknown,
    ccArmsman, ccCabalist, ccCleric, ccFriar, ccInfiltrator, ccMercenary,
    ccMinstrel, ccPaladin, ccScout, ccSorcerer, ccTheurgist, ccWizard,
    ccBard, ccBlademaster, ccChampion, ccDruid, ccEldritch, ccEnchanter,
    ccHero, ccMentalist, ccNightshade, ccRanger, ccWarden,
    ccBerserker, ccHealer, ccHunter, ccRunemaster, ccShadowblade, ccShaman,
    ccSkald, ccSpiritmaster, ccThane, ccWarrior);

function DAOCCharacterClassToStr(AClass: TDAOCCharacterClass) : string;

implementation

function DAOCCharacterClassToStr(AClass: TDAOCCharacterClass) : string;
begin
  case AClass of
    ccUnknown: Result := '';
    ccArmsman: Result := 'Armsman';
    ccCabalist: Result := 'Cabalist';
    ccCleric: Result := 'Cleric';
    ccFriar: Result := 'Friar';
    ccInfiltrator: Result := 'Infiltrator';
    ccMercenary: Result := 'Mercenary';
    ccMinstrel: Result := 'Minstrel';
    ccPaladin: Result := 'Paladin';
    ccScout: Result := 'Scout';
    ccSorcerer: Result := 'Sorcerer';
    ccTheurgist: Result := 'Theurgist';
    ccWizard: Result := 'Wizard';
    ccBard: Result := 'Bard';
    ccBlademaster: Result := 'Blademaster';
    ccChampion: Result := 'Champion';
    ccDruid: Result := 'Druid';
    ccEldritch: Result := 'Eldritch';
    ccEnchanter: Result := 'Enchanter';
    ccHero: Result := 'Hero';
    ccMentalist: Result := 'Mentalist';
    ccNightshade: Result := 'Nightshade';
    ccRanger: Result := 'Ranger';
    ccWarden: Result := 'Warden';
    ccBerserker: Result := 'Berserker';
    ccHealer: Result := 'Healer';
    ccHunter: Result := 'Hunter';
    ccRunemaster: Result := 'Runemaster';
    ccShadowblade: Result := 'Shadowblade';
    ccShaman: Result := 'Shaman';
    ccSkald: Result := 'Skald';
    ccSpiritmaster: Result := 'Spiritmaster';
    ccThane: Result := 'Thane';
    ccWarrior: Result := 'Warrior';
    else
      Result := 'CharClass ' + IntToStr(ord(AClass));
  end;
end;

end.
