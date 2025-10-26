namespace ParticleLenia

/// 2D point.
type Point =
    { X : float; Y : float }

    /// Origin.
    static member Zero = { X = 0; Y = 0 }

    /// Negates a point.
    static member inline (~-)(a) =
        { X = -a.X; Y = -a.Y }
    
    /// Adds two points component-wise.
    static member inline (+)(p1, p2) =
        { X = p1.X + p2.X; Y = p1.Y + p2.Y }
    
    /// Subtracts one point from another component-wise.
    static member inline (-)(p1, p2) =
        { X = p1.X - p2.X; Y = p1.Y - p2.Y }
    
    /// Multiplies a point by a scalar.
    static member inline (*)(p, a) =
        { X = p.X * a; Y = p.Y * a }
    
    /// Multiplies a scalar by a point.
    static member inline (*)(a, p) =
        { X = a * p.X; Y = a * p.Y }
    
    /// Multiplies one point by another component-wise.
    static member inline (*)(p1, p2) =
        { X = p1.X * p2.X; Y = p1.Y * p2.Y }
    
    /// Divides a point by a scalar.
    static member inline (/)(p, a) =
        { X = p.X / a; Y = p.Y / a }

    /// Computes the length of a point when considered
    /// as a vector.
    member this.Length =
        sqrt (this.X * this.X + this.Y * this.Y)

module Point =

    /// Creates a point.
    let create x y =
        { X = x; Y = y }
