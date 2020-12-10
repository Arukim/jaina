namespace Jaina.Logic.Blueprints

open Aicup2020.Model

module Military = 
    let Default = [
        EntityType.RangedUnit;
        EntityType.MeleeUnit;
        EntityType.RangedUnit;
        EntityType.MeleeUnit;
    ]

    let Defensive = [
        EntityType.RangedUnit;
        EntityType.RangedUnit;
    ]

