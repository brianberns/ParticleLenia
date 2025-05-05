// https://observablehq.com/@znah/particle-lenia-from-scratch

open System
open System.IO

open SkiaSharp

let parms =
    {|
        mu_k = 4.0
        sigma_k = 1.0
        w_k = 0.022
        mu_g = 0.6
        sigma_g = 0.15
        c_rep = 1.0
        dt = 0.1
    |}

let point_n = 200

let random = Random(0)

let points =
    Array.init (2 * point_n) (fun _ ->
        (random.NextDouble() - 0.5) * 12.0)

let fields =
    {|
        R_val = Array.zeroCreate<float> point_n
        U_val = Array.zeroCreate<float> point_n
        R_grad = Array.zeroCreate<float> (point_n*2)
        U_grad = Array.zeroCreate<float> (point_n*2)
    |}

let add_xy (a : _[]) i x y c =
    a[i*2] <- a[i*2] + x*c
    a[i*2+1] <- a[i*2+1] + y*c

let repulsion_f x c_rep =
    let t = max (1.0 - x) 0.0
    0.5*c_rep*t*t, -c_rep*t

let peak_f x mu sigma w =
    let t = (x-mu)/sigma
    let y = w / exp(t*t)
    y, -2.0*t*y/sigma

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
    fill R_val (repulsion_f 0.0 c_rep |> fst)
    fill U_val (peak_f 0.0 mu_k sigma_k w_k |> fst)
    fill R_grad 0; fill U_grad 0

    for i = 0 to point_n-2 do
        for j = i + 1 to point_n-1 do
            let mutable rx = points[i*2]   - points[j*2]
            let mutable ry = points[i*2+1] - points[j*2+1]
            let r = sqrt(rx*rx + ry*ry) + 1e-20
            rx <- rx / r; ry <- ry / r  // ∇r = [rx, ry]
  
            if r < 1.0 then
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

let step () =

    let R_val = fields.R_val
    let U_val = fields.U_val
    let R_grad = fields.R_grad
    let U_grad = fields.U_grad

    let mu_g = parms.mu_g
    let sigma_g = parms.sigma_g
    let dt = parms.dt

    compute_fields()
    let mutable total_E = 0.0
    for i = 0 to point_n - 1 do
        let G, dG = peak_f U_val[i] mu_g sigma_g 1.0
        // [vx, vy] = -∇E = G'(U)∇U - ∇R
        let vx = dG*U_grad[i*2]   - R_grad[i*2]
        let vy = dG*U_grad[i*2+1] - R_grad[i*2+1]
        add_xy points i vx vy dt
        total_E <- total_E + R_val[i] - G
    total_E / float point_n

let stepsPerFrame = 5
let world_width = 25.0

let animate (outputDir: string) frameIndex =
    for _ in 1 .. stepsPerFrame do
        step () |> ignore

    let width, height = 800, 800 // Canvas size
    let surface = SKSurface.Create(SKImageInfo(width, height))
    let canvas = surface.Canvas

    // Reset and clear canvas
    canvas.Clear(SKColors.White)
    canvas.Translate(float32 width / 2.0f, float32 height / 2.0f)
    let scale = float width / world_width
    canvas.Scale(float32 scale, float32 scale)

    // Draw particles
    use paint = new SKPaint()
    paint.Color <- SKColors.Black
    paint.Style <- SKPaintStyle.Stroke
    paint.StrokeWidth <- 0.1f

    for i = 0 to point_n - 1 do
        let x = points[i * 2]
        let y = points[i * 2 + 1]
        let r = parms.c_rep / (fields.R_val[i] * 5.0) // Calculate radius based on repulsion
        canvas.DrawCircle(float32 x, float32 y, float32 r, paint)

    // Save the frame as a PNG file
    use image = surface.Snapshot()
    use data = image.Encode(SKEncodedImageFormat.Png, 100)
    let filePath = Path.Combine(outputDir, sprintf "frame_%04d.png" frameIndex)
    use stream = File.Create(filePath)
    data.SaveTo(stream)
    
    printfn "Saved %s" filePath

let outputDir = "Output"
Directory.CreateDirectory(outputDir) |> ignore
for frameIndex in 1 .. 1000 do
    animate outputDir frameIndex

// C:\users\brian\Downloads\ffmpeg-7.1.1-essentials_build\bin\ffmpeg.exe -framerate 30 -i "frame_%04d.png" -c:v libx264 -pix_fmt yuv420p output.mp4
