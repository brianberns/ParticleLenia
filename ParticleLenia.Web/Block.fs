namespace ParticleLenia.Web

/// Rectangular block.
type Block =
    {
        /// Center point.
        Center : Point

        /// Extent.
        Size : Point

        /// Is mobile?
        Mobile : bool
    }

    /// Start corner.
    member this.Start =
        this.Center - (this.Size / 2.0)

    /// Finish corner.
    member this.Finish =
        this.Center + (this.Size / 2.0)

module Block =

    /// Creates a block.
    let create center size mobile =
        {
            Center = center
            Size = size
            Mobile = mobile
        }
