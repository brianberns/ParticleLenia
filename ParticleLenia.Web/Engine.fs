namespace ParticleLenia.Web

module Array =

    /// Fast array initialization.
    let inline init count initializer =
        let result = Array.zeroCreate count
        for i = 0 to count - 1 do
            result[i] <- initializer i
        result

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
    /// given points.
    let get_fields (points : Point[]) =

            // compute the upper triangle of the lookup table
        let nPoints = points.Length
        let upper =
            Array.init nPoints (fun i ->
                Array.init (nPoints - i) (fun j ->
                    let diff = points[i] - points[j + i]   // compute actual j from offset
                    let r = diff.Length + 1e-20
                    let dr = diff / r                      // ∇r
                    let R, dR = repulsion c_rep r          // ∇R = R'(r) ∇r
                    let K, dK = peak mu_k sigma_k w_k r    // ∇K = K'(r) ∇r
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

            // compute fields from each point's contribution
        Array.init nPoints (fun i ->
            let vs = Array.init nPoints (lookup i)
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
            {|
                R_grad = R_grad; R_val = R_val
                U_grad = U_grad; U_val = U_val
                dG = dG; G = G
            |})

    let step points =

        let fields = get_fields points
        let points =
            (points, fields)
                ||> Array.map2 (fun point field ->
                    let vpt = field.dG * field.U_grad - field.R_grad   // v = -∇E = G'(U)∇U - ∇R
                    point + (dt * vpt))
        points, fields
