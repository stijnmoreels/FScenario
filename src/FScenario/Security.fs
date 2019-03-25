namespace Security

open System
open System.Security.Cryptography.X509Certificates

module Certificate =

    /// Creates a certificate store with given certificates but revert the added certificates 
    /// by removing them from the store and disposing them when the returned disposable gets disposed.
    let store name location certs =
        let s = new X509Store ((name : StoreName), (location : StoreLocation))
        List.iter (fun (c : X509Certificate2) -> s.Add c) certs

        Disposable.create <| fun () ->
            using s (fun s -> 
                Seq.iter (fun c -> s.Remove c; c.Dispose ()) certs)