namespace ParticleLenia.Web

module Array =

    /// Fast array initialization.
    let inline init count initializer =
        let result = Array.zeroCreate count
        for i = 0 to count - 1 do
            result[i] <- initializer i
        result

module Engine =

    let repulsion c_rep x =
        let t = max (1.0 - x) 0.0
        0.5 * c_rep * t * t, -c_rep * t

    let peak mu sigma w x =
        let t = (x - mu) / sigma
        let y = w / exp (t * t)
        y, -2.0 * t * y / sigma

    let mu_k = 4.0
    let sigma_k = 1.0
    let w_k = 0.022
    let mu_g = 0.6
    let sigma_g = 0.15
    let c_rep = 1.0
    let dt = 0.1

    let get_fields (points : Point[]) =

        let nPoints = points.Length
        let upper =
            Array.init nPoints (fun i ->
                Array.init (nPoints - i) (fun j ->
                    let diff = points[i] - points[j + i]
                    let r = diff.Length + 1e-20
                    let dr = diff / r                     // ∇r
                    let R, dR = repulsion c_rep r         // ∇R = R'(r) ∇r
                    let K, dK = peak mu_k sigma_k w_k r   // ∇K = K'(r) ∇r
                    {|
                        R = R; dR = dR * dr
                        K = K; dK = dK * dr
                    |}))
        let lookup i j =
            if i <= j then upper[i][j - i]
            else
                let v = upper[j][i - j]
                {| v with dR = -v.dR; dK = -v.dK |}

        Array.init nPoints (fun i ->
            let vs = Array.init nPoints (lookup i)
            let R_grad = vs |> Array.sumBy _.dR
            let R_val = vs |> Array.sumBy _.R
            let U_grad = vs |> Array.sumBy _.dK
            let U_val = vs |> Array.sumBy _.K
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
