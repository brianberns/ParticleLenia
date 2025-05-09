namespace ParticleLenia.Web

open System

open Browser
open Browser.Types
open Fable.Core.JsInterop

module Program =

        // initialize canvas
    let canvas =
        document.getElementById "canvas"
            :?> HTMLCanvasElement

        // initialize drawing context
    let ctx = canvas.getContext_2d()
    ctx.lineWidth <- 0.05

        // handle block movement
    let blockSpeed = 0.2
    let mutable blockVelocity = Point.Zero
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

    /// Number of engine time steps per frame.
    let stepsPerFrame = 5

    /// Width of the Lenia world to draw.
    let worldWidth = 40.0

    /// Squeeze factor due to repulsion.
    let squeeze = 5.0

    /// Full circle.
    let two_pi = 2.0 * Math.PI

    /// Minimum and maximum expected energy levels.
    let E_min = -0.5
    let E_max = 0.8

    /// Gets a color representing the given energy level.
    let getColor E =
        let E = min (max E E_min) E_max
        let E_norm = (E + E_min) / (E_max - E_min)
        let hue = (360.0 - 60.0) * E_norm + 60.0   // from yellow (60.0) to red (360.0)
        !^($"hsl({hue}, 100%%, 50%%)")

    let drawBlock (block : Block) =
        ctx.beginPath()
        let start = block.Start
        let size = block.Size
        ctx.rect(start.X, start.Y, size.X, size.Y)
        ctx.fillStyle <- !^"black"
        ctx.fill()

    let drawParticle point field =

        ctx.beginPath()

            // draw each particle as a circle
        let r = Engine.c_rep / (field.R_val * squeeze)   // smaller circle represents a particle being "squeezed"
        ctx.arc(point.X, point.Y, r, 0.0, two_pi)

            // fill the circle
        ctx.fillStyle <-
            getColor (field.R_val - field.G)
        ctx.fill()

            // draw the circle's border
        ctx.stroke()

    /// Animates one frame.
    let animateFrame world =

            // move the block
        let world =
            let block = world.Blocks[0]
            let block =
                { block with
                    Center = block.Center + blockVelocity }
            { world with Blocks = [| block |]}

            // move the particles
        let world, fields =
            ((world, Array.empty), [1 .. stepsPerFrame])
                ||> Seq.fold (fun (world, _) _ ->
                    Engine.step world)

            // prepare to draw
        ctx.clearRect(0, 0, canvas.width, canvas.height)
        ctx.translate(canvas.width / 2.0, canvas.height / 2.0)
        let s = canvas.width / worldWidth
        ctx.scale(s, s)

            // draw the block
        drawBlock world.Blocks[0]

            // draw each particle
        Array.iter2 drawParticle world.Particles fields

            // reset transform
        ctx.setTransform(1.0, 0.0, 0.0, 1.0, 0.0, 0.0)

        world

    /// Animation loop.
    let animate world =

        let check = 100

        let rec loop iFrame prev world =
            window.requestAnimationFrame(fun timestamp ->
                let cur =
                    if iFrame % check = 0 then
                        console.log(
                            $"%.3f{float check * 1000.0 / (timestamp - prev)} frames/sec")
                        timestamp
                    else prev
                animateFrame world
                    |> loop (iFrame + 1) cur)
                    |> ignore

        loop 1 0.0 world

        // random number generator
    let random =
        let seed = DateTime.Now.Millisecond
        console.log($"Random seed: {seed}")
        Random(seed)

    /// Makes the given number of particles.
    let makeParticles numParticles scale offset =
        Array.init numParticles (fun _ ->
            {
                X = (random.NextDouble()) * scale.X + offset.X
                Y = (random.NextDouble()) * scale.Y + offset.Y
            })

        // initial particle locations
    let particles =
        let n = 50
        let scaleX = 8.0
        let scaleY = 8.0
        let offsetX = 3.5
        let offsetY = 2.5
        [|
            yield! makeParticles n
                (Point.create scaleX scaleY)
                (Point.create offsetX offsetY)
            yield! makeParticles n
                (Point.create -scaleX scaleY)
                (Point.create -offsetX offsetY)
            yield! makeParticles n
                (Point.create scaleX -scaleY)
                (Point.create offsetX -offsetY)
            yield! makeParticles n
                (Point.create -scaleX -scaleY)
                (Point.create -offsetX -offsetY)
        |]

        // movable blocks
    let block =
        let center = Point.Zero
        let size =
            let width = worldWidth / 8.0
            Point.create width width
        Block.create center size

    World.create particles [|block|]
        |> animate
