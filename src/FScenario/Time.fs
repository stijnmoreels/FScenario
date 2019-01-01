namespace System

module TimeInt =
    let _500ms = 500
    let _1s = 1000
    let _5s = 5000
    let _10s = 10_000
    let _1min = 60_000
    let _5min = 300_000

module TimeString =
    let _500ms = "00:00:00.5"
    let _1s = "00:00:01"
    let _5s = "00:00:05"
    let _10s = "00:00:10"
    let _1min = "00:01:00"
    let _5min = "00:05:00"

[<AutoOpen>]
module TimeSpans = 
    let _500ms = TimeSpan.FromMilliseconds 500.
    let  _1s = TimeSpan.FromSeconds 1.
    let _5s = TimeSpan.FromSeconds 5.
    let _10s = TimeSpan.FromSeconds 10.
    let _30s = TimeSpan.FromSeconds 30.
    let _1min = TimeSpan.FromMinutes 1.
    let _5min = TimeSpan.FromMinutes 5.

    type TimeSpan with
        static member _500ms = TimeSpan.FromMilliseconds 500.
        static member  _1s = TimeSpan.FromSeconds 1.
        static member _5s = TimeSpan.FromSeconds 5.
        static member _10s = TimeSpan.FromSeconds 10.
        static member _30s = TimeSpan.FromSeconds 30.
        static member _1min = TimeSpan.FromMinutes 1.
        static member _5min = TimeSpan.FromMinutes 5.

