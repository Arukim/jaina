namespace Jaina.Logic.Blueprints

open Aicup2020.Model

module Economics = 
    let Default = [
        // inital state
        [EntityType.BuilderBase, 1;
         EntityType.RangedBase, 1;
         EntityType.MeleeBase, 1;]
        // power-up builders
        [EntityType.BuilderUnit, 7];
        // get some houses
        [EntityType.House, 2;
          EntityType.BuilderUnit, 11];
        [EntityType.House, 4;
            EntityType.BuilderUnit, 15];
        [EntityType.House, 6;
          EntityType.BuilderUnit, 20;];
        [EntityType.House, 9;
          EntityType.BuilderUnit, 30;];
        [EntityType.House, 14;
          EntityType.BuilderUnit, 40;];
        [EntityType.House, 19;
          EntityType.BuilderUnit, 50;];
        [EntityType.House, 24;
          EntityType.BuilderUnit, 60;];
        [EntityType.House, 27;
          EntityType.BuilderUnit, 66;];
        [EntityType.RangedBase, 2];
        [EntityType.MeleeBase, 2];
    ] 

