namespace ParticleLenia.Web

open Browser
open Browser.Types

open ParticleLenia

module Canvas =

        // initialize canvas
    let canvas =
        document.getElementById "canvas"
            :?> HTMLCanvasElement

        // initialize drawing context
    let ctx = canvas.getContext_2d()
    ctx.lineWidth <- 0.05

    /// Size of the Lenia world to draw.
    let worldWidth = 40.0
    let worldHeight =
        canvas.height * worldWidth / canvas.width

        // initialize reset button
    let mutable reset = false
    let btnReset =
        document.getElementById "reset"
            :?> HTMLButtonElement
    btnReset.onclick <- (fun _ -> reset <- true)

        // initialize block button
    let btnBlock =
        document.getElementById "block"
            :?> HTMLInputElement

    /// Number of engine time steps per frame.
    let stepsPerFrame = 5

    /// Animates one frame.
    let animateFrame world =

            // move mobile blocks
        let world = Block.step world

            // move particles
        let world, fields =
            ((world, Array.empty), [1 .. stepsPerFrame])
                ||> Seq.fold (fun (world, _) _ ->
                    Engine.step world)

            // prepare to draw
        ctx.clearRect(0, 0, canvas.width, canvas.height)
        ctx.translate(canvas.width / 2.0, canvas.height / 2.0)
        let s = canvas.width / worldWidth
        ctx.scale(s, s)

            // draw the blocks
        Array.iter
            (Block.draw ctx)
            world.Blocks

            // draw each particle
        Array.iter2
            (Particle.draw ctx)
            world.Particles fields

            // reset transform
        ctx.setTransform(1.0, 0.0, 0.0, 1.0, 0.0, 0.0)

        world

    /// Log framerate.
    let logFramerate check iFrame prev cur =
        if iFrame % check = 0 then
            console.log(
                $"%.3f{float check * 1000.0 / (cur - prev)} frames/sec")
            cur
        else prev

    /// Animation loop.
    let animate () =

        let createWorld () =
            Web.World.create
                worldWidth
                worldHeight
                btnBlock.``checked``

        let rec loop iFrame prev world =
            window.requestAnimationFrame(fun timestamp ->
                let cur =
                    logFramerate 100 iFrame prev timestamp
                let world =
                    if reset then
                        reset <- false
                        createWorld ()
                    else world
                animateFrame world
                    |> loop (iFrame + 1) cur)
                |> ignore

        loop 1 0.0 (createWorld ())
