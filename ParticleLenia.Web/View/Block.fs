namespace ParticleLenia.Web

open Browser
open Browser.Types
open Fable.Core.JsInterop

open ParticleLenia

module Block =

        // handle block movement
    let private blockSpeed = 0.2
    let mutable private blockVelocity = Point.Zero
    window.onkeydown <- fun evt ->
        blockVelocity <-
            match evt.key with
                | "ArrowUp"    -> Point.create 0.0 -blockSpeed
                | "ArrowDown"  -> Point.create 0.0  blockSpeed
                | "ArrowLeft"  -> Point.create -blockSpeed 0.0
                | "ArrowRight" -> Point.create  blockSpeed 0.0
                | _ -> blockVelocity
    window.onkeyup <- fun evt ->
        blockVelocity <- Point.Zero

    /// Moves mobile blocks.
    let step world =
        let blocks =
            world.Blocks
                |> Array.map (fun block ->
                    if block.Mobile then
                        { block with
                            Center = block.Center + blockVelocity }
                    else block)
        { world with Blocks = blocks }

    /// Draws the given block.
    let draw (ctx : CanvasRenderingContext2D) (block : Block) =
        ctx.beginPath()
        let start = block.Start
        let size = block.Size
        ctx.rect(start.X, start.Y, size.X, size.Y)
        ctx.fillStyle <- !^"black"
        ctx.fill()
