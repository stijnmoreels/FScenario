namespace Security

open System
open System.Security.Cryptography.X509Certificates

module Certificate =

    /// Creates a certificate store with given certificates but revert the added certificates 
    /// by removing them from the store and disposing them when the returned disposable gets disposed.
    let store name location certs =
        let s = new X509Store ((name : StoreName), (location : StoreLocation))
        for c in certs do s.Add c

        Disposable.create <| fun () ->
            using (fun (s : X509Store) -> 
              for c in certs do
                s.Remove c
                c.Dispose ()) s