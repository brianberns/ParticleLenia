namespace ParticleLenia.Web

open System

open Browser.Types
open Fable.Core.JsInterop

open ParticleLenia

module Particle =

    /// Makes the given number of particles.
    let makeParticles (random : Random) numParticles scale offset =
        Array.init numParticles (fun _ ->
            let r = random.NextDouble()
            let theta = 2.0 * Math.PI * random.NextDouble()
            let point = Point.create (r * cos theta) (r * sin theta)
            point * scale + offset)

    /// Squeeze factor due to repulsion.
    let squeeze = 5.0

    /// Full circle.
    let two_pi = 2.0 * Math.PI

    /// Minimum and maximum expected energy levels.
    let E_min = -0.5
    let E_max = 0.8

    /// Gets a color representing the given energy level.
    let getColor E =
        let E = min (max E E_min) E_max
        let E_norm = (E + E_min) / (E_max - E_min)
        let hue = (360.0 - 60.0) * E_norm + 60.0   // from yellow (60.0) to red (360.0)
        !^($"hsl({hue}, 100%%, 50%%)")

    /// Draws the given particle.
    let draw (ctx : CanvasRenderingContext2D) point field =

        ctx.beginPath()

            // draw each particle as a circle (smaller circle represents "squeeze" due to repulsion)
        let r = Engine.c_rep / (field.R_val * squeeze)
        ctx.arc(point.X, point.Y, r, 0.0, two_pi)

            // fill the circle
        ctx.fillStyle <- getColor (field.R_val - field.G)
        ctx.fill()

            // draw the circle's border
        ctx.stroke()
