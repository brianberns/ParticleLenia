namespace ParticleLenia.Web

open System
open Browser
open ParticleLenia

module World =

    /// Creates a movable block.
    let createMovableBlock worldWidth =
        let center = Point.Zero
        let size =
            let blockWidth = worldWidth / 8.0
            Point.create blockWidth blockWidth
        Block.create center size true

    /// Creates fixed blocks.
    let createFixedBlocks worldWidth worldHeight thickness =
        let blockWidth = worldWidth + 2.0 * thickness
        let blockHeight = worldHeight + 2.0 * thickness
        [|
                // left
            Block.create
                (Point.create ((-worldWidth - thickness) / 2.0) 0.0)
                (Point.create thickness blockHeight)
                false

                // right
            Block.create
                (Point.create ((worldWidth + thickness) / 2.0) 0.0)
                (Point.create thickness blockHeight)
                false

                // bottom
            Block.create
                (Point.create 0.0 ((-worldHeight - thickness) / 2.0))
                (Point.create blockWidth thickness)
                false

                // top
            Block.create
                (Point.create 0.0 ((worldHeight + thickness) / 2.0))
                (Point.create blockWidth thickness)
                false
        |]

    let createParticles () =

            // random number generator
        let random =
            let seed = DateTime.Now.Millisecond
            console.log($"Random seed: {seed}")
            Random(seed)

            // initial particle locations
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

    /// Creates a world.
    let create width height block =

            // create blocks
        let movableBlock = createMovableBlock width
        let fixedBlocks =
            let thickness = 2.0 * movableBlock.Size.X
            createFixedBlocks width height thickness

            // create particles
        let particles = createParticles ()

            // create and animate world
        World.create particles [|
            if block then movableBlock
            yield! fixedBlocks
        |]
