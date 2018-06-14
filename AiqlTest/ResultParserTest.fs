namespace AiqlTest

open Xunit
open Xunit.Abstractions
open System.Net.Http
open System
open ExpressionBuilder.ResultParer
open Newtonsoft.Json

type Request () =
    member val timestamp = Unchecked.defaultof<DateTime> with get, set
    member val id = Unchecked.defaultof<string> with get, set
    member val source = Unchecked.defaultof<string> with get, set
    member val name = Unchecked.defaultof<string> with get, set
    member val url = Unchecked.defaultof<string> with get, set
    member val success = Unchecked.defaultof<string> with get, set
    member val resultCode = Unchecked.defaultof<string> with get, set
    member val duration = Unchecked.defaultof<float> with get, set
    member val performanceBucket = Unchecked.defaultof<string> with get, set
    member val customDimensions = Unchecked.defaultof<string> with get, set
    member val customMeasurements = Unchecked.defaultof<string> with get, set
    member val operation_Name = Unchecked.defaultof<string> with get, set
    member val operation_Id = Unchecked.defaultof<string> with get, set
    member val operation_ParentId = Unchecked.defaultof<string> with get, set
    member val operation_SyntheticSource = Unchecked.defaultof<string> with get, set
    member val session_Id = Unchecked.defaultof<string> with get, set
    member val user_Id = Unchecked.defaultof<string> with get, set
    member val user_AuthenticatedId = Unchecked.defaultof<string> with get, set
    member val user_AccountId = Unchecked.defaultof<string> with get, set
    member val application_Version = Unchecked.defaultof<string> with get, set
    member val client_Type = Unchecked.defaultof<string> with get, set
    member val client_Model = Unchecked.defaultof<string> with get, set
    member val client_OS = Unchecked.defaultof<string> with get, set
    member val client_IP = Unchecked.defaultof<string> with get, set
    member val client_City = Unchecked.defaultof<string> with get, set
    member val client_StateOrProvince = Unchecked.defaultof<string> with get, set
    member val client_CountryOrRegion = Unchecked.defaultof<string> with get, set
    member val client_Browser = Unchecked.defaultof<string> with get, set
    member val cloud_RoleName = Unchecked.defaultof<string> with get, set
    member val cloud_RoleInstance = Unchecked.defaultof<string> with get, set
    member val appId = Unchecked.defaultof<string> with get, set
    member val appName = Unchecked.defaultof<string> with get, set
    member val iKey = Unchecked.defaultof<string> with get, set
    member val sdkVersion = Unchecked.defaultof<string> with get, set
    member val itemId = Unchecked.defaultof<string> with get, set
    member val itemType = Unchecked.defaultof<string> with get, set
    member val itemCount = Unchecked.defaultof<int> with get, set
    

module ResultParserTest = 
    open Newtonsoft.Json.Linq

    [<Fact>]
    let canParseResult () = 
        
        let filePath = "sampleAiqlResult.json"
        use fileStream = System.IO.File.OpenRead filePath
        let resultSequence = readResults<Request>(fileStream)
        Assert.All(resultSequence, fun elem -> Assert.NotEqual( Unchecked.defaultof<DateTime>, elem.timestamp ))


