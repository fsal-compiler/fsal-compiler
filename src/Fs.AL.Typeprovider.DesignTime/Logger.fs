module internal Fs.AL.Typeprovider.DesignTime.Logger

open System
open System.IO
open System.Threading

let log =
    
    let logmsg =
        let home = Environment.GetFolderPath(Environment.SpecialFolder.UserProfile)
        let log = Path.Combine(home, "typeproviderlog.txt")
        fun (msg: string) ->
            let message =  
                let time = DateTime.Now.ToString("dd/MM/yyyy HH:mm:ss.fff")
                sprintf "[%s]: %s" time msg
            use mutex = new Mutex(false, "typeproviderlog.txt")
            mutex.WaitOne(TimeSpan.FromSeconds(3.0)) |> ignore
            File.AppendAllLines(log, [message])
            mutex.ReleaseMutex()
    
    #if DEBUG
    fun (msg: string) -> ()
//    fun (msg: string) -> logmsg msg
    #else
    fun (msg: string) -> ()
//    fun (msg: string) -> logmsg msg
//    fun (msg: string) -> logmsg msg
    #endif