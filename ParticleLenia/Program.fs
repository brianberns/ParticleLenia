open DiffSharp

/// Computes the value of a Gaussian peak at the given point with
/// the given mean and standard deviation.
let peak_f (x : Tensor) (mu : float (*mean*)) (sigma : float (*std dev*)) =
    exp(-((x - mu) / sigma) ** 2)

let mu_k = 4.0
let sigma_k = 1.0
let w_k = 0.022
let mu_g = 0.6
let sigma_g = 0.15
let c_rep = 1.0

let square x = x * x

let fields_f (points : Tensor) (x : Tensor) =
    let r = sqrt(square(x-points).sum(-1).clamp(1e-10))
    let U = (peak_f r mu_k sigma_k).sum()*w_k
    let G = peak_f U mu_g sigma_g
    let R = c_rep/2.0 * ((1.0-r).clamp(0.0)**2).sum()
    {| U=U; G=G; R=R; E=R-G |}

let motion_f points =
    -dsharp.grad (fun x -> (fields_f points x).E) points

let points0 =
    (dsharp.rand([200; 2]) - 0.5) * 12.0
let dt = 0.1

let odeint_euler f x0 dt n =
    let step_f x _ =
        x+dt*(f x)
    Seq.scan step_f x0 [1..n]

let rotor_story = odeint_euler motion_f points0 dt 10000
printfn "%A" rotor_story
