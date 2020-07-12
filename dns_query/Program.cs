using System;
namespace dns_query
{
    class Program
    { 
        private static string LookupDns(string s)
        {
            try
            {
                System.Net.IPHostEntry ip = System.Net.Dns.GetHostEntry(s);
 
                string result = ip.AddressList[0].ToString();
 
                for (int i = 1; i < ip.AddressList.Length; ++i)
                    result += ", " + ip.AddressList[i].ToString();
 
                return result;
            }
            catch (System.Net.Sockets.SocketException se)
            {
                return se.Message;
            }
        }
        static void Main(string[] args)
        {
            Console.WriteLine(LookupDns("ttay.me"));
        }
    }
}
