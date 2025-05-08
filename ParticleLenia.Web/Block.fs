namespace ParticleLenia.Web

type Block =
    {
        Center : Point
        Size : Point
    }

    member this.Start =
        this.Center - (this.Size / 2.0)

    member this.Finish =
        this.Center + (this.Size / 2.0)

module Block =

    let create center size =
        {
            Center = center
            Size = size
        }
