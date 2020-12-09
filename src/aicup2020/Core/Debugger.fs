namespace Jaina.Core

open Aicup2020.Model

type IDebugger = 
    abstract member Refresh : unit -> unit
    abstract member Events : DebugCommand list with get
    abstract member DrawLine : Vec2Int -> Vec2Int -> Palette -> unit
    abstract member FillCellA : Palette -> single -> Vec2Int -> unit

type Debugger() =
    static let instance = new Debugger()

    let mutable events: DebugCommand list = []

    let screenOffset = {X=0.f;Y=0.f} : Vec2Single

    interface IDebugger with
        member this.Refresh() =
            events <- []
            ()

        member this.DrawLine a b color = 
            events <- DebugCommand.Add {
                Data = DebugData.Primitives {
                    Vertices = [|{
                        WorldPos = Some((Vec2Single.fromVec2Int a) |> Vec2Single.ToCenter)
                        ScreenOffset = screenOffset
                        Color = color |> ColorSingle.from 0.5f
                    };
                    {
                       WorldPos = Some((Vec2Single.fromVec2Int b) |> Vec2Single.ToCenter)
                       ScreenOffset = screenOffset
                       Color = color |> ColorSingle.from 0.5f
                   }|]
                    PrimitiveType = PrimitiveType.Lines
                }
            } :: events
        member this.Events with get() = events

        member this.FillCellA color a pos =
            let vpos = Vec2Single.fromVec2Int pos
            events <- DebugCommand.Add {
                          Data = DebugData.Primitives {
                              Vertices = [|{
                                      WorldPos = Some(vpos)
                                      ScreenOffset = screenOffset
                                      Color = color |> ColorSingle.from a
                                  };
                                  {
                                     WorldPos = Some(vpos |> Vec2Single.toCornerSE)
                                     ScreenOffset = screenOffset
                                     Color = color |> ColorSingle.from a
                                 };
                                 {
                                    WorldPos = Some(vpos |> Vec2Single.toCornerNE)
                                    ScreenOffset = screenOffset
                                    Color = color |> ColorSingle.from a
                                 };
                                 {
                                     WorldPos = Some(vpos)
                                     ScreenOffset = screenOffset
                                     Color = color |> ColorSingle.from a
                                 };
                                 {
                                    WorldPos = Some(vpos |> Vec2Single.toCornerNE)
                                    ScreenOffset = screenOffset
                                    Color = color |> ColorSingle.from a
                                 };
                                 {
                                    WorldPos = Some(vpos |> Vec2Single.toCornerNW)
                                    ScreenOffset = screenOffset
                                    Color = color |> ColorSingle.from a
                                 }|]
                              PrimitiveType = PrimitiveType.Triangles
                          }
                      } :: events

type NullDebugger() =
    interface IDebugger with
           member this.Refresh() = ()
           member this.DrawLine _ _ _ = ()
           member this.FillCellA _ _ _ = ()

           member this.Events with get() = []

type Debug() = 
    static let instance = 
        #if DEBUG
            new Debugger()
        #else
            new NullDebugger()
        #endif
    static member Instance with get() = instance :> IDebugger