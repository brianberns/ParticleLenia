namespace ParticleLenia.Web

open System
open Browser
open ParticleLenia

module World =

    /// Creates a movable block.
    let createMovableBlock worldWidth =
        let blockWidth = worldWidth / 8.0
        let center =
            Point.create
                (-3.2 * blockWidth)
                ( 2.2 * blockWidth)
        let size =
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

    /// Creates particles.
    let createParticles numParticles quadrants =

            // random number generator
        let random =
            let seed = DateTime.Now.Millisecond
            console.log($"Random seed: {seed}")
            Random(seed)

            // initial particle locations
        if quadrants then
            let make offsetX offsetY =
                let scale = Point.create 2.0 2.0
                Particle.makeParticles
                    random (numParticles / 4) scale
                    (Point.create offsetX offsetY)
            let offsetX, offsetY = 6.5, 5.5
            [|
                yield! make  offsetX  offsetY
                yield! make -offsetX  offsetY
                yield! make  offsetX -offsetY
                yield! make -offsetX -offsetY
            |]
        else
            let factor = float numParticles / 22.0
            let scale = Point.create factor factor
            Particle.makeParticles random numParticles
                scale Point.Zero

    /// Creates a world.
    let create width height numParticles block quadrants =

            // create blocks
        let movableBlock = createMovableBlock width
        let fixedBlocks =
            let thickness = 2.0 * movableBlock.Size.X
            createFixedBlocks width height thickness

            // create particles
        let particles =
            createParticles numParticles quadrants

            // create and animate world
        World.create particles [|
            if block then movableBlock
            yield! fixedBlocks
        |]
