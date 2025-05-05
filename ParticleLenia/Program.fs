open DiffSharp

/// Computes the value of a Gaussian peak at the given point with
/// the given mean and standard deviation.
let peak_f (x : Tensor) (mu : float (*mean*)) (sigma : float (*std dev*)) =
    exp -(((x-mu)/sigma)**2)

let mu_k = 4.0
let sigma_k = 1.0
let w_k = 0.022
let mu_g = 0.6
let sigma_g = 0.15
let c_rep = 1.0

let square x = x * x

type Fields =
    {
        U : Tensor
        G : Tensor
        R : Tensor
        E : Tensor
    }

let fields_f (points : Tensor) (x : Tensor) =
    let r = sqrt(square(x-points).sum(-1).clamp(1e-10))
    let U = (peak_f r mu_k sigma_k).sum()*w_k
    let G = peak_f U mu_g sigma_g
    let R = c_rep/2.0 * ((1.0-r).clamp(0.0)**2).sum()
    { U=U; G=G; R=R; E=R-G }

let vmap f (inputs : Tensor) =
    [|
        for i = 0 to inputs.shape[0] - 1 do
            yield f inputs[i]
    |]

let vmap_tensor f =
    vmap f >> dsharp.stack

let vmap2 f (inputs : Tensor) =
    [|
        for i = 0 to inputs.shape[0] - 1 do
            [|
                for j = 0 to inputs.shape[1] - 1 do
                    yield f inputs[i, j]
            |]
    |]

let motion_f points =
    let grad_E = dsharp.grad (fun x -> (fields_f points x).E)
    -(vmap_tensor grad_E points)

let points0 =
    (dsharp.rand([200; 2]) - 0.5) * 12.0
let dt = 0.1

let odeint_euler f x0 dt n =
    let step_f x _ =
        x+dt*(f x)
    Seq.scan step_f x0 [1..n]

type VideoWriter(fps, filename) =
    class end

let mgrid (startX : float) (stopX : float) numX (startY : float) (stopY : float) numY =
    let x = dsharp.linspace(startX, stopX, numX)
    let y = dsharp.linspace(startY, stopY, numY)
    let xGrid = x.unsqueeze(0).expand([numY; numX])
    let yGrid = y.unsqueeze(1).expand([numY; numX])
    dsharp.stack([xGrid; yGrid], dim=2)

let show_lenia points extent =
    let w = 400
    let xy = mgrid -1.0 1.0 w -1.0 1.0 w * extent
    let e0 = -peak_f (dsharp.tensor 0.0) mu_g sigma_g
    let f = fields_f points
    let fields = vmap2 f xy
    ()

let animate_lenia tracks name =
    let vid = VideoWriter(60, name)
    let extent = dsharp.abs(tracks).max()*1.2
    let bar_len = tracks.shape[0]
    for i = 0 to bar_len - 1 do
        let points = tracks[i]
        if i % 10 = 0 then
            let img = show_lenia points extent
            ()

let rotor_story =
    odeint_euler motion_f points0 dt 1
        |> dsharp.stack
animate_lenia rotor_story "rotor.mp4"
