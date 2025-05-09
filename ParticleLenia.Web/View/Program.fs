namespace ParticleLenia.Web

open System

open Browser
open Browser.Types

open ParticleLenia

module Program =

        // initialize canvas
    let canvas =
        document.getElementById "canvas"
            :?> HTMLCanvasElement

        // initialize drawing context
    let ctx = canvas.getContext_2d()
    ctx.lineWidth <- 0.05

    /// Number of engine time steps per frame.
    let stepsPerFrame = 5

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

    /// Creates a world.
    let createWorld () =

            // movable block
        let block =
            let center = Point.Zero
            let size =
                let width = worldWidth / 8.0
                Point.create width width
            Block.create center size true

            // immobile blocks
        let blocks =
            let thickness = 2.0 * block.Size.X
            let width = worldWidth + 2.0 * thickness
            let height = worldHeight + 2.0 * thickness
            [|
                    // left
                Block.create
                    (Point.create ((-worldWidth - thickness) / 2.0) 0.0)
                    (Point.create thickness height)
                    false

                    // right
                Block.create
                    (Point.create ((worldWidth + thickness) / 2.0) 0.0)
                    (Point.create thickness height)
                    false

                    // bottom
                Block.create
                    (Point.create 0.0 ((-worldHeight - thickness) / 2.0))
                    (Point.create width thickness)
                    false

                    // top
                Block.create
                    (Point.create 0.0 ((worldHeight + thickness) / 2.0))
                    (Point.create width thickness)
                    false
            |]

            // random number generator
        let random =
            let seed = DateTime.Now.Millisecond
            console.log($"Random seed: {seed}")
            Random(seed)

            // initial particle locations
        let particles =
            let n = 50
            let scaleX, scaleY = 8.0, 8.0
            let offsetX, offsetY = 3.5, 2.5
            let make = Particle.makeParticles random n
            [|
                yield! make
                    (Point.create scaleX scaleY)
                    (Point.create offsetX offsetY)
                yield! make
                    (Point.create -scaleX scaleY)
                    (Point.create -offsetX offsetY)
                yield! make
                    (Point.create scaleX -scaleY)
                    (Point.create offsetX -offsetY)
                yield! make
                    (Point.create -scaleX -scaleY)
                    (Point.create -offsetX -offsetY)
            |]

            // create and animate world
        World.create particles [|
            if btnBlock.``checked`` then block
            yield! blocks
        |]

    /// Log framerate.
    let logFramerate check iFrame prev cur =
        if iFrame % check = 0 then
            console.log(
                $"%.3f{float check * 1000.0 / (cur - prev)} frames/sec")
            cur
        else prev

    /// Animation loop.
    let animate () =

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

    animate ()
