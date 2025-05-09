namespace ParticleLenia.Web

open System

open Browser
open Browser.Types

open ParticleLenia

module World =

    /// Creates a world.
    let create width height block =

            // movable block
        let movableBlock =
            let center = Point.Zero
            let size =
                let blockWidth = width / 8.0
                Point.create blockWidth blockWidth
            Block.create center size true

            // fixed blocks
        let fixedBlocks =
            let thickness = 2.0 * movableBlock.Size.X
            let blockWidth = width + 2.0 * thickness
            let blockHeight = height + 2.0 * thickness
            [|
                    // left
                Block.create
                    (Point.create ((-width - thickness) / 2.0) 0.0)
                    (Point.create thickness blockHeight)
                    false

                    // right
                Block.create
                    (Point.create ((width + thickness) / 2.0) 0.0)
                    (Point.create thickness blockHeight)
                    false

                    // bottom
                Block.create
                    (Point.create 0.0 ((-height - thickness) / 2.0))
                    (Point.create blockWidth thickness)
                    false

                    // top
                Block.create
                    (Point.create 0.0 ((height + thickness) / 2.0))
                    (Point.create blockWidth thickness)
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
            if block then movableBlock
            yield! fixedBlocks
        |]
