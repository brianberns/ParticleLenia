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
        dt = 0.2
    |}

let point_n = 400

let random = Random(0)

type Point =
    { X : float; Y : float }

    static member Zero = { X = 0; Y = 0 }

    static member (+)(a, b) =
        { X = a.X + b.X; Y = a.Y + b.Y }

    static member (-)(a, b) =
        { X = a.X - b.X; Y = a.Y - b.Y }

    static member (*)(a, b) =
        { X = a * b.X; Y = a * b.Y }

    static member (/)(a, b) =
        { X = a.X / b; Y = a.Y / b }

let points =
    let coord () = (random.NextDouble() - 0.5) * 36.0
    Array.init point_n (fun _ ->
        { X = coord (); Y = coord () })

let fields =
    {|
        R_val = Array.zeroCreate<float> point_n
        U_val = Array.zeroCreate<float> point_n
        R_grad = Array.zeroCreate<Point> point_n
        U_grad = Array.zeroCreate<Point> point_n
    |}

let add_xy (a : _[]) i pt c =
    a[i] <- { X = a[i].X + pt.X*c; Y = a[i].Y + pt.Y*c }

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
    fill R_grad Point.Zero; fill U_grad Point.Zero

    for i = 0 to point_n-2 do
        for j = i + 1 to point_n-1 do
            let mutable rpt = points[i] - points[j]
            let r = sqrt(rpt.X*rpt.X + rpt.Y*rpt.Y) + 1e-20
            rpt <- rpt / r  // ∇r = [rx, ry]
  
            if r < 1.0 then
                // ∇R = R'(r) ∇r
                let R, dR = repulsion_f r c_rep
                add_xy R_grad i rpt  dR
                add_xy R_grad j rpt -dR
                R_val[i] <- R_val[i] + R; R_val[j] <- R_val[j] + R

            // ∇K = K'(r) ∇r
            let K, dK = peak_f r mu_k sigma_k w_k
            add_xy U_grad i rpt  dK
            add_xy U_grad j rpt -dK
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
        let vpt = dG*U_grad[i] - R_grad[i]
        add_xy points i vpt dt
        total_E <- total_E + R_val[i] - G
    total_E / float point_n

let stepsPerFrame = 10
let world_width = 40.0

let animate (outputDir: string) frameIndex =
    for _ in 1 .. stepsPerFrame do
        step () |> ignore

    let width, height = 1000, 1000 // Canvas size
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
        let pt = points[i]
        let r = parms.c_rep / (fields.R_val[i] * 5.0) // Calculate radius based on repulsion
        canvas.DrawCircle(float32 pt.X, float32 pt.Y, float32 r, paint)

    // Save the frame as a PNG file
    use image = surface.Snapshot()
    use data = image.Encode(SKEncodedImageFormat.Png, 100)
    let filePath = Path.Combine(outputDir, sprintf "frame_%04d.png" frameIndex)
    use stream = File.Create(filePath)
    data.SaveTo(stream)
    
    printfn "Saved %s" filePath

let outputDir = "Output"
Directory.CreateDirectory(outputDir) |> ignore
for frameIndex in 1 .. 10000 do
    animate outputDir frameIndex

// C:\users\brian\Downloads\ffmpeg-7.1.1-essentials_build\bin\ffmpeg.exe -framerate 30 -i "frame_%04d.png" -c:v libx264 -pix_fmt yuv420p output.mp4
