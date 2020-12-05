namespace Jaina.Core

open Aicup2020.Model

module Config =
    let Resource_Tile_Walk_Price = 7u
    let CombatUnit_Tile_Walk_Price = 4u
    let BuilderUnit_Tile_Walk_Price = 5u
    let Attack_Map_Refresh_Rate = 7
    let Builders_Base_Limit = 11
    let Builders_Per_House = 2
    let Max_Houses = 30
    let Map_Size = 80
    let Global_Attack_Targets = [{X= Map_Size - 1; Y = Map_Size - 1}; {X=0; Y = Map_Size - 1}; {X = Map_Size - 1; Y = 0}]
    let Build_Warrior_Watermark = 90
    let Architect_House_Watermark = 60
    let RepairAssistDistance = 3
    let RepairAssistMaxCount = 2