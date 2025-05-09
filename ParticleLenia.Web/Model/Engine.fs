namespace ParticleLenia

/// World of objects to animate.
type World =
    {
        Particles : Point[]
        Blocks : Block[]
    }

module World =

    /// Creates a world.
    let create particles blocks =
        {
            Particles = particles
            Blocks = blocks
        }

/// R, U, and G fields, and their gradients.
type Field =
    {
        R_grad : Point
        R_val : float
        U_grad : Point
        U_val : float
        dG : float
        G : float
    }

module Engine =

    /// Value and slope of repulsive field at the
    /// given distance.
    // https://www.desmos.com/calculator/l4ebnqi2ul
    let repulsion c_rep r =
        let a = max (1.0 - r) 0.0
        0.5 * c_rep * a * a, -c_rep * a

    /// Value of a Gaussian peak at the given point with
    /// the given mean and standard deviation.
    // https://www.desmos.com/calculator/ppen2zyilc
    let peak mu sigma w x =
        let a = (x - mu) / sigma
        let b = w * exp -(a * a)
        b, -2.0 * a * b / sigma

    /// Lenia field mean.
    let mu_k = 4.0

    /// Lenia field standard deviation.
    let sigma_k = 1.0

    /// Lenia field scale.
    let w_k = 0.022

    /// Growth field mean.
    let mu_g = 0.6

    /// Growth standard deviation.
    let sigma_g = 0.15

    /// Repulsion strength.
    let c_rep = 1.0

    /// Time step.
    let dt = 0.05

    /// Computes the value and gradient of each field for the
    /// given particles.
    let computeFields (particles : Point[]) =

            // compute the upper triangle of the lookup table
        let nParticles = particles.Length
        let upper =
            Array.init nParticles (fun i ->
                Array.init (nParticles - i) (fun j ->
                    let diff = particles[i] - particles[j + i]   // compute actual j from offset
                    let r = diff.Length + 1e-20
                    let dr = diff / r                     // ∇r
                    let R, dR = repulsion c_rep r         // ∇R = R'(r) ∇r
                    let K, dK = peak mu_k sigma_k w_k r   // ∇K = K'(r) ∇r
                    {|
                        R = R; dR = dR * dr
                        K = K; dK = dK * dr
                    |}))

            // full lookup table
        let lookup i j =
            if i <= j then upper[i][j - i]
            else
                let v = upper[j][i - j]
                {| v with dR = -v.dR; dK = -v.dK |}   // same field strength, but opposite gradient

            // compute fields from each particle's contribution
        Array.init nParticles (fun i ->
            let vs = Array.init nParticles (lookup i)
            let mutable R_grad = Point.Zero
            let mutable R_val = 0.0
            let mutable U_grad = Point.Zero
            let mutable U_val = 0.0
            for v in vs do
                R_grad <- R_grad + v.dR
                R_val <- R_val + v.R
                U_grad <- U_grad + v.dK
                U_val <- U_val + v.K
            let G, dG = peak mu_g sigma_g 1.0 U_val
            {
                R_grad = R_grad; R_val = R_val
                U_grad = U_grad; U_val = U_val
                dG = dG; G = G
            })

    /// Pushes the given point out of the given block, if
    /// necessary.
    let push (block : Block) point =

            // overlap amounts
        let left = point.X - block.Start.X
        let right = block.Finish.X - point.X
        let bottom = point.Y - block.Start.Y
        let top = block.Finish.Y - point.Y

            // point is inside block?
        if left > 0.0 && right > 0.0
            && bottom > 0.0 && top > 0.0 then

            let dx =
                if left < right then -left
                else right
            let dy =
                if bottom < top then -bottom
                else top

                // push by smallest amount in one direction only
            let dx, dy =
                if abs dx < abs dy then dx, 0.0
                else 0.0, dy

                // push strength
            let factor = 0.3
            point + Point.create (factor * dx) (factor * dy)

        else point

    /// Moves the particles in the given world one time step
    /// forward against the energy gradient.
    let step world =

            // move particles forward
        let fields = computeFields world.Particles
        let particles =
            (world.Particles, fields)
                ||> Array.map2 (fun particle field ->
                    let v = field.dG * field.U_grad - field.R_grad   // v = -∇E = G'(U)∇U - ∇R
                    particle + (dt * v))
        let particles =
            (particles, world.Blocks)
                ||> Array.fold (fun particles block ->
                    Array.map (push block) particles)

            // update world
        let world =
            { world with Particles = particles }
        world, fields
