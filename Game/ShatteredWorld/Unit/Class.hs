
module Game.ShatteredWorld.Unit.Class(
  UnitClass(..),
  unitClassSuffixCandidates
  ) where

data UnitClass = UKnight | UArcher | UMage | UTower
               deriving(Show, Read, Eq)

unitClassSuffixCandidates :: UnitClass -> [String]
unitClassSuffixCandidates UKnight   = [ "Rider", "Cavalier", "Raider", "Bogatyr"
                                      , "Horseman", "Condottiere", "Hidalgo", "Mauler"
                                      , "Yojimbo", "Ronin", "Warrior", "Dragoon"
                                      , "Paladin", "Lancer", "Warlord", "Flanker"
                                      , "Hussar", "Hackapell", "Rodelero", "Mameluke"
                                      , "Spahi", "Caballero", "Stradiot", "Elmeti"
                                      , "Mahout", "Centurion", "Hitokiri", "Cataphract"
                                      , "Klibanophoroi", "Hetairoi", "Equites", "Patricius"
                                      ]
unitClassSuffixCandidates UArcher   = [ "Fletcher", "Bowman", "Hunter", "Ranger"
                                      , "Slayer", "Bowyer", "Yeoman", "Longbowman"
                                      , "Kyudoka", "Shooter", "Poacher", "Sharpshooter"
                                      , "Rogue", "Woodsman", "Marskman", "Avenger"
                                      , "Jaeger", "Conscript", "Recruit", "Militia"
                                      , "Chukonu", "Skirmisher", "Toxotes", "Janissary"
                                      , "Yumi", "Keshik", "Shashu", "Arcus"
                                      , "Atlatl", "Arbalester", "Boleadoras", "Regular"
                                      ]
unitClassSuffixCandidates UMage     = [ "Enchanter", "Wizard", "Warlock", "Conjurer"
                                      , "Professor", "Adept", "Magister", "Speaker"
                                      , "Palver", "Scryer", "Sensei", "Thaumaturge"
                                      , "Meditant", "Augur", "Sophont", "Master"
                                      , "Mahoutsukai", "Sylph", "Shyde", "Pharmakeia"
                                      , "Benandanti", "Striga", "Occultist", "Invoker"
                                      , "Mediator", "Gematriarch", "Spellcaster", "Abjurer"
                                      , "Illusionist", "Monist", "Dreamer", "Polymath"
                                      ]
unitClassSuffixCandidates UTower    = [ "Bailey", "Ward", "Turret", "Rampart"
                                      , "Defender", "Protector", "Fort", "Keep"
                                      , "Castle", "Rocca", "Stronghold", "Bastion"
                                      , "Watch", "Fortalice", "Gatehouse", "Castrum"
                                      , "Ostrog", "Blockhouse", "Burg", "Shiro"
                                      , "Donjon", "Palisade", "Turris", "Parapet"
                                      , "Enceinte", "Redoubt", "Lunette", "Crannog"
                                      , "Citadel", "Outpost", "Barbican", "Bergfried"
                                      ]

