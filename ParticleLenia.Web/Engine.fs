namespace ParticleLenia.Web

type Settings =
    {
        mu_k : float
        sigma_k : float
        w_k : float
        mu_g : float
        sigma_g : float
        c_rep : float
        dt : float
    }

module Engine =

    let repulsion c_rep x =
        let t = max (1.0 - x) 0.0
        0.5 * c_rep * t * t, -c_rep * t

    let peak mu sigma w x =
        let t = (x - mu)/sigma
        let y = w / exp (t * t)
        y, -2.0 * t * y / sigma

    let get_fields settings (points : Point[]) =

        let c_rep = settings.c_rep
        let mu_k = settings.mu_k
        let sigma_k = settings.sigma_k
        let w_k = settings.w_k
        let mu_g = settings.mu_g
        let sigma_g = settings.sigma_g

        let nPoints = points.Length
        let upper =
            Array.Parallel.init nPoints (fun i ->
                [|
                    for j = i to nPoints - 1 do
                        let diff = points[i] - points[j]
                        let r = diff.Length + 1e-20
                        let dr = diff / r                     // ∇r
                        let R, dR = repulsion c_rep r         // ∇R = R'(r) ∇r
                        let K, dK = peak mu_k sigma_k w_k r   // ∇K = K'(r) ∇r
                        {|
                            R = R; dR = dR * dr
                            K = K; dK = dK * dr
                        |}
                |])
        let lookup i j =
            if i <= j then upper[i][j - i]
            else
                let v = upper[j][i - j]
                {| v with dR = -v.dR; dK = -v.dK |}

        [|
            for i = 0 to nPoints - 1 do
                let vs =
                    [| for j = 0 to nPoints - 1 do lookup i j |]
                let U_val = vs |> Array.sumBy _.K
                let G, dG = peak mu_g sigma_g 1.0 U_val
                {|
                    R_grad = vs |> Array.sumBy _.dR
                    R_val = vs |> Array.sumBy _.R
                    U_grad = vs |> Array.sumBy _.dK
                    U_val = U_val
                    dG = dG
                    G = G
                |}
        |]

    let step settings points =

        let dt = settings.dt

        let fields = get_fields settings points
        let points =
            (points, fields)
                ||> Array.map2 (fun point field ->
                    let vpt = field.dG * field.U_grad - field.R_grad   // v = -∇E = G'(U)∇U - ∇R
                    point + (dt * vpt))
        points, fields
