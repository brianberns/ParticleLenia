// https://observablehq.com/@znah/particle-lenia-from-scratch

open System

let parms =
    {|
        mu_k = 4.0f
        sigma_k = 1.0f
        w_k = 0.022f
        mu_g = 0.6f
        sigma_g = 0.15f
        c_rep = 1.0f
        dt = 0.1f
    |}

let point_n = 200

let random = Random(0)

let points =
    Array.init (2 * point_n) (fun _ ->
        (random.NextSingle() - 0.5f) * 12.0f)

let fields =
    {|
        R_val = Array.zeroCreate<float32> point_n
        U_val = Array.zeroCreate<float32> point_n
        R_grad = Array.zeroCreate<float32> (point_n*2)
        U_grad = Array.zeroCreate<float32> (point_n*2)
    |}

let add_xy (a : _[]) i x y c =
    a[i*2] <- a[i*2] + x*c
    a[i*2+1] <- a[i*2+1] + y*c

let repulsion_f x c_rep =
    let t = max (1.0f - x) 0.0f
    0.5f*c_rep*t*t, -c_rep*t

let peak_f x mu sigma w =
    let t = (x-mu)/sigma
    let y = w / exp(t*t)
    y, -2.0f*t*y/sigma

let fill (arr : _[]) x =
    for i = 0 to arr.Length - 1 do
        arr[i] <- x

let compute_fields() =

    let R_val = fields.R_val
    let U_val = fields.U_val
    let R_grad = fields.R_grad
    let U_grad = fields.U_grad

    let c_rep = parms.c_rep
    let mu_k = parms.mu_k
    let sigma_k = parms.sigma_k
    let w_k = parms.w_k

    // account for the own field of each particle
    fill R_val (repulsion_f 0.0f c_rep |> snd)
    fill U_val (peak_f 0.0f mu_k sigma_k w_k |> snd)
    fill R_grad 0f; fill U_grad 0f

    for i = 0 to point_n-2 do
        for j = i + 1 to point_n-1 do
            let mutable rx = points[i*2]   - points[j*2]
            let mutable ry = points[i*2+1] - points[j*2+1]
            let r = sqrt(rx*rx + ry*ry) + 1e-20f
            rx <- rx / r; ry <- ry / r  // ∇r = [rx, ry]
  
            if (r < 1.0f) then
                // ∇R = R'(r) ∇r
                let R, dR = repulsion_f r c_rep
                add_xy R_grad i rx ry  dR
                add_xy R_grad j rx ry -dR
                R_val[i] <- R_val[i] + R; R_val[j] <- R_val[j] + R

            // ∇K = K'(r) ∇r
            let K, dK = peak_f r mu_k sigma_k w_k
            add_xy U_grad i rx ry  dK
            add_xy U_grad j rx ry -dK
            U_val[i] <- U_val[i] + K; U_val[j] <- U_val[j] + K
