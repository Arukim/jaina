namespace Jaina.Core

open Aicup2020.Model

module Config =
    let Resource_Tile_Walk_Price = 4u
    let Attack_Map_Refresh_Rate = 7
    let Builders_Base_Limit = 8
    let Builders_Per_House = 2
    let Map_Size = 80
    let Global_Attack_Target = {X= Map_Size - 1; Y = Map_Size - 1}
    let Build_Warrior_Watermark = 90
    let Architect_House_Watermark = 60