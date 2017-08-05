namespace AiqlTest

open Xunit
open Xunit.Abstractions
open System.Net.Http
open System
open AiqlContract.ResultParer
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

    let readData<'T> filePath = 
        use fileStream = System.IO.File.OpenRead filePath
        use streamReader = new System.IO.StreamReader(fileStream)
        use jsonReader =  new JsonTextReader(streamReader)
        readResults<'T> jsonReader

    [<Fact>]
    let canParseResult () = 
        let expected = 
           JArray.Parse(
             """
              [
                  {"timestamp":"2017-07-29T21:03:48.613Z","id":"|J6c+jHKUwWk=.eda6d914_","source":null,"name":"GET Reports/Index","url":"http://fabrikamfiberapp.azurewebsites.net/Reports","success":"True","resultCode":"200","duration":4.484,"performanceBucket":"<250ms","customDimensions":"{\"_MS.ProcessedByMetricExtractors\":\"(Name:'Requests', Ver:'1.0')\"}","customMeasurements":null,"operation_Name":"GET Reports/Index","operation_Id":"J6c+jHKUwWk=","operation_ParentId":"J6c+jHKUwWk=","operation_SyntheticSource":"Application Insights Availability Monitoring","session_Id":"a2f8f7e3-9e91-4502-b294-e83723647f98","user_Id":"us-va-ash-azr_a2f8f7e3-9e91-4502-b294-e83723647f98","user_AuthenticatedId":null,"user_AccountId":null,"application_Version":"AutoGen_49c3aea0-4641-4675-93b5-55f7a62d22d3","client_Type":"PC","client_Model":null,"client_OS":null,"client_IP":"13.106.106.0","client_City":"","client_StateOrProvince":"","client_CountryOrRegion":"United States","client_Browser":null,"cloud_RoleName":"fabrikamfiberapp","cloud_RoleInstance":"RD00155DA96753","appId":"cf58dcfd-0683-487c-bc84-048789bca8e5","appName":"fabrikamprod","iKey":"5a2e4e0c-e136-4a15-9824-90ba859b0a89","sdkVersion":"web:2.4.1-1362","itemId":"7d945802-74a1-11e7-8749-2fc2fb943018","itemType":"request","itemCount":1},
                  {"timestamp":"2017-07-29T21:04:01.613Z","id":"|boo1HKxwCzQ=.eda6d915_","source":null,"name":"GET Home/Index","url":"http://fabrikamfiberapp.azurewebsites.net/","success":"True","resultCode":"200","duration":682.7867,"performanceBucket":"500ms-1sec","customDimensions":"{\"_MS.ProcessedByMetricExtractors\":\"(Name:'Requests', Ver:'1.0')\"}","customMeasurements":null,"operation_Name":"GET Home/Index","operation_Id":"boo1HKxwCzQ=","operation_ParentId":"boo1HKxwCzQ=","operation_SyntheticSource":"Bot","session_Id":"","user_Id":"","user_AuthenticatedId":null,"user_AccountId":null,"application_Version":"AutoGen_49c3aea0-4641-4675-93b5-55f7a62d22d3","client_Type":"PC","client_Model":null,"client_OS":null,"client_IP":"0.0.0.0","client_City":"","client_StateOrProvince":"","client_CountryOrRegion":"","client_Browser":null,"cloud_RoleName":"fabrikamfiberapp","cloud_RoleInstance":"RD00155DA96753","appId":"cf58dcfd-0683-487c-bc84-048789bca8e5","appName":"fabrikamprod","iKey":"5a2e4e0c-e136-4a15-9824-90ba859b0a89","sdkVersion":"web:2.4.1-1362","itemId":"7d945803-74a1-11e7-8749-2fc2fb943018","itemType":"request","itemCount":1},
                  {"timestamp":"2017-07-29T21:04:44.463Z","id":"|bNub0Q6Ccdg=.eda6d916_","source":null,"name":"GET Home/Index","url":"http://fabrikamfiberapp.azurewebsites.net/","success":"True","resultCode":"200","duration":679.1902,"performanceBucket":"500ms-1sec","customDimensions":"{\"_MS.ProcessedByMetricExtractors\":\"(Name:'Requests', Ver:'1.0')\"}","customMeasurements":null,"operation_Name":"GET Home/Index","operation_Id":"bNub0Q6Ccdg=","operation_ParentId":"bNub0Q6Ccdg=","operation_SyntheticSource":"Application Insights Availability Monitoring","session_Id":"a059463a-7c54-4247-9c59-cc7bd8d8c5ef","user_Id":"emea-fr-pra-edge_a059463a-7c54-4247-9c59-cc7bd8d8c5ef","user_AuthenticatedId":null,"user_AccountId":null,"application_Version":"AutoGen_49c3aea0-4641-4675-93b5-55f7a62d22d3","client_Type":"PC","client_Model":null,"client_OS":null,"client_IP":"94.245.72.0","client_City":"","client_StateOrProvince":"","client_CountryOrRegion":"United Kingdom","client_Browser":null,"cloud_RoleName":"fabrikamfiberapp","cloud_RoleInstance":"RD00155DA96753","appId":"cf58dcfd-0683-487c-bc84-048789bca8e5","appName":"fabrikamprod","iKey":"5a2e4e0c-e136-4a15-9824-90ba859b0a89","sdkVersion":"web:2.4.1-1362","itemId":"9dd729d0-74a1-11e7-9384-2fe7737a5ecd","itemType":"request","itemCount":1},
                  {"timestamp":"2017-07-29T21:04:46.897Z","id":"|ND5SBGHUIzc=.eda6d917_","source":null,"name":"GET Home/Index","url":"http://fabrikamfiberapp.azurewebsites.net/","success":"True","resultCode":"200","duration":747.7313,"performanceBucket":"500ms-1sec","customDimensions":"{\"_MS.ProcessedByMetricExtractors\":\"(Name:'Requests', Ver:'1.0')\"}","customMeasurements":null,"operation_Name":"GET Home/Index","operation_Id":"ND5SBGHUIzc=","operation_ParentId":"ND5SBGHUIzc=","operation_SyntheticSource":"Application Insights Availability Monitoring","session_Id":"327fb7e3-add7-45d3-90e5-9f6127a9bb94","user_Id":"latam-br-gru-edge_327fb7e3-add7-45d3-90e5-9f6127a9bb94","user_AuthenticatedId":null,"user_AccountId":null,"application_Version":"AutoGen_49c3aea0-4641-4675-93b5-55f7a62d22d3","client_Type":"PC","client_Model":null,"client_OS":null,"client_IP":"65.54.66.0","client_City":"São Paulo","client_StateOrProvince":"Sao Paulo","client_CountryOrRegion":"Brazil","client_Browser":null,"cloud_RoleName":"fabrikamfiberapp","cloud_RoleInstance":"RD00155DA96753","appId":"cf58dcfd-0683-487c-bc84-048789bca8e5","appName":"fabrikamprod","iKey":"5a2e4e0c-e136-4a15-9824-90ba859b0a89","sdkVersion":"web:2.4.1-1362","itemId":"9dd729d1-74a1-11e7-9384-2fe7737a5ecd","itemType":"request","itemCount":1},
                  {"timestamp":"2017-07-29T21:04:48.093Z","id":"|P0XvLj428/4=.eda6d918_","source":null,"name":"GET Home/Index","url":"http://fabrikamfiberapp.azurewebsites.net/","success":"True","resultCode":"200","duration":663.9103,"performanceBucket":"500ms-1sec","customDimensions":"{\"_MS.ProcessedByMetricExtractors\":\"(Name:'Requests', Ver:'1.0')\"}","customMeasurements":null,"operation_Name":"GET Home/Index","operation_Id":"P0XvLj428/4=","operation_ParentId":"P0XvLj428/4=","operation_SyntheticSource":"Application Insights Availability Monitoring","session_Id":"5f825a7f-00f3-4016-99e4-ff474bf9a61f","user_Id":"emea-ru-msa-edge_5f825a7f-00f3-4016-99e4-ff474bf9a61f","user_AuthenticatedId":null,"user_AccountId":null,"application_Version":"AutoGen_49c3aea0-4641-4675-93b5-55f7a62d22d3","client_Type":"PC","client_Model":null,"client_OS":null,"client_IP":"94.245.82.0","client_City":"","client_StateOrProvince":"","client_CountryOrRegion":"United Kingdom","client_Browser":null,"cloud_RoleName":"fabrikamfiberapp","cloud_RoleInstance":"RD00155DA96753","appId":"cf58dcfd-0683-487c-bc84-048789bca8e5","appName":"fabrikamprod","iKey":"5a2e4e0c-e136-4a15-9824-90ba859b0a89","sdkVersion":"web:2.4.1-1362","itemId":"9dd729d2-74a1-11e7-9384-2fe7737a5ecd","itemType":"request","itemCount":1},
                  {"timestamp":"2017-07-29T21:05:03.773Z","id":"|rzp294sFPSY=.eda6d919_","source":null,"name":"GET Home/Index","url":"http://fabrikamfiberapp.azurewebsites.net/","success":"True","resultCode":"200","duration":704.8444,"performanceBucket":"500ms-1sec","customDimensions":"{\"_MS.ProcessedByMetricExtractors\":\"(Name:'Requests', Ver:'1.0')\"}","customMeasurements":null,"operation_Name":"GET Home/Index","operation_Id":"rzp294sFPSY=","operation_ParentId":"rzp294sFPSY=","operation_SyntheticSource":"Application Insights Availability Monitoring","session_Id":"d7b9a395-1a39-4e21-a41d-c7841b32f8f4","user_Id":"emea-nl-ams-azr_d7b9a395-1a39-4e21-a41d-c7841b32f8f4","user_AuthenticatedId":null,"user_AccountId":null,"application_Version":"AutoGen_49c3aea0-4641-4675-93b5-55f7a62d22d3","client_Type":"PC","client_Model":null,"client_OS":null,"client_IP":"213.199.178.0","client_City":"Amsterdam","client_StateOrProvince":"North Holland","client_CountryOrRegion":"Netherlands","client_Browser":null,"cloud_RoleName":"fabrikamfiberapp","cloud_RoleInstance":"RD00155DA96753","appId":"cf58dcfd-0683-487c-bc84-048789bca8e5","appName":"fabrikamprod","iKey":"5a2e4e0c-e136-4a15-9824-90ba859b0a89","sdkVersion":"web:2.4.1-1362","itemId":"9dd729d3-74a1-11e7-9384-2fe7737a5ecd","itemType":"request","itemCount":1},
                  {"timestamp":"2017-07-29T21:11:51.821Z","id":"|oCRf/O/iTGY=.a64e5f81_","source":null,"name":"GET Employees/Create","url":"http://aiconnect2.cloudapp.net/FabrikamProd/Employees/Create","success":"False","resultCode":"404","duration":1.5364,"performanceBucket":"<250ms","customDimensions":"{\"_MS.ProcessedByMetricExtractors\":\"(Name:'Requests', Ver:'1.0')\"}","customMeasurements":null,"operation_Name":"GET Employees/Create","operation_Id":"oCRf/O/iTGY=","operation_ParentId":"oCRf/O/iTGY=","operation_SyntheticSource":"Application Insights Availability Monitoring","session_Id":"f8af6d0a-1a71-4818-b43b-8579b0ad4841","user_Id":"apac-sg-sin-azr_f8af6d0a-1a71-4818-b43b-8579b0ad4841","user_AuthenticatedId":null,"user_AccountId":null,"application_Version":"","client_Type":"PC","client_Model":null,"client_OS":null,"client_IP":"52.187.30.0","client_City":"Wilmington","client_StateOrProvince":"Delaware","client_CountryOrRegion":"United States","client_Browser":null,"cloud_RoleName":"","cloud_RoleInstance":"AIConnect2","appId":"cf58dcfd-0683-487c-bc84-048789bca8e5","appName":"fabrikamprod","iKey":"5a2e4e0c-e136-4a15-9824-90ba859b0a89","sdkVersion":"web:2.4.1-1362","itemId":"9c8a4206-74a2-11e7-8e48-55b423f37797","itemType":"request","itemCount":1},
                  {"timestamp":"2017-07-29T21:12:01.957Z","id":"|z7RQhwwcJZU=.eda6d935_","source":null,"name":"GET Home/Index","url":"http://fabrikamfiberapp.azurewebsites.net/","success":"True","resultCode":"200","duration":751.9332,"performanceBucket":"500ms-1sec","customDimensions":"{\"_MS.ProcessedByMetricExtractors\":\"(Name:'Requests', Ver:'1.0')\"}","customMeasurements":null,"operation_Name":"GET Home/Index","operation_Id":"z7RQhwwcJZU=","operation_ParentId":"z7RQhwwcJZU=","operation_SyntheticSource":"Application Insights Availability Monitoring","session_Id":"c7e76b4c-8d13-4a03-acea-23388f98f096","user_Id":"emea-ru-msa-edge_c7e76b4c-8d13-4a03-acea-23388f98f096","user_AuthenticatedId":null,"user_AccountId":null,"application_Version":"AutoGen_49c3aea0-4641-4675-93b5-55f7a62d22d3","client_Type":"PC","client_Model":null,"client_OS":null,"client_IP":"94.245.82.0","client_City":"","client_StateOrProvince":"","client_CountryOrRegion":"United Kingdom","client_Browser":null,"cloud_RoleName":"fabrikamfiberapp","cloud_RoleInstance":"RD00155DA96753","appId":"cf58dcfd-0683-487c-bc84-048789bca8e5","appName":"fabrikamprod","iKey":"5a2e4e0c-e136-4a15-9824-90ba859b0a89","sdkVersion":"web:2.4.1-1362","itemId":"a3035cce-74a2-11e7-b077-99bead63380e","itemType":"request","itemCount":1},
                  {"timestamp":"2017-07-29T21:12:39.315Z","id":"|sKR26meLDx8=.eda6d936_","source":null,"name":"GET Home/Index","url":"http://fabrikamfiberapp.azurewebsites.net/","success":"True","resultCode":"200","duration":698.447,"performanceBucket":"500ms-1sec","customDimensions":"{\"_MS.ProcessedByMetricExtractors\":\"(Name:'Requests', Ver:'1.0')\"}","customMeasurements":null,"operation_Name":"GET Home/Index","operation_Id":"sKR26meLDx8=","operation_ParentId":"sKR26meLDx8=","operation_SyntheticSource":"Application Insights Availability Monitoring","session_Id":"98132504-8f15-4a24-b560-098926e04630","user_Id":"us-ca-sjc-azr_98132504-8f15-4a24-b560-098926e04630","user_AuthenticatedId":null,"user_AccountId":null,"application_Version":"AutoGen_49c3aea0-4641-4675-93b5-55f7a62d22d3","client_Type":"PC","client_Model":null,"client_OS":null,"client_IP":"207.46.98.0","client_City":"San Jose","client_StateOrProvince":"California","client_CountryOrRegion":"United States","client_Browser":null,"cloud_RoleName":"fabrikamfiberapp","cloud_RoleInstance":"RD00155DA96753","appId":"cf58dcfd-0683-487c-bc84-048789bca8e5","appName":"fabrikamprod","iKey":"5a2e4e0c-e136-4a15-9824-90ba859b0a89","sdkVersion":"web:2.4.1-1362","itemId":"beea7590-74a2-11e7-b077-99bead63380e","itemType":"request","itemCount":1},
                  {"timestamp":"2017-07-29T21:12:41.726Z","id":"|YVCv6WbZGs0=.eda6d937_","source":null,"name":"GET Reports/Index","url":"http://fabrikamfiberapp.azurewebsites.net/Reports","success":"True","resultCode":"200","duration":7.5181,"performanceBucket":"<250ms","customDimensions":"{\"_MS.ProcessedByMetricExtractors\":\"(Name:'Requests', Ver:'1.0')\"}","customMeasurements":null,"operation_Name":"GET Reports/Index","operation_Id":"YVCv6WbZGs0=","operation_ParentId":"YVCv6WbZGs0=","operation_SyntheticSource":"Application Insights Availability Monitoring","session_Id":"d28e9719-5d88-4d2e-aa31-9cb54546ad03","user_Id":"latam-br-gru-edge_d28e9719-5d88-4d2e-aa31-9cb54546ad03","user_AuthenticatedId":null,"user_AccountId":null,"application_Version":"AutoGen_49c3aea0-4641-4675-93b5-55f7a62d22d3","client_Type":"PC","client_Model":null,"client_OS":null,"client_IP":"65.54.66.0","client_City":"São Paulo","client_StateOrProvince":"Sao Paulo","client_CountryOrRegion":"Brazil","client_Browser":null,"cloud_RoleName":"fabrikamfiberapp","cloud_RoleInstance":"RD00155DA96753","appId":"cf58dcfd-0683-487c-bc84-048789bca8e5","appName":"fabrikamprod","iKey":"5a2e4e0c-e136-4a15-9824-90ba859b0a89","sdkVersion":"web:2.4.1-1362","itemId":"beea7591-74a2-11e7-b077-99bead63380e","itemType":"request","itemCount":1}
              ]
             """
             )
        let filePath = "sampleAiqlResult.json"
        use fileStream = System.IO.File.OpenRead filePath
        use resultReader = new ResultReader(fileStream)
        let actual = resultReader.ReadResults<Request>() |> Seq.toArray |> JArray.FromObject
        Assert.Equal<JArray>(expected,actual)


