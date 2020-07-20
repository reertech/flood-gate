{ proxyConfig = { configPort = (8080 : Natural)
                , configTargetHost = "www.etsy.com"
                , configTargetPort = 443
                , configIsTargetTls = True
                , configMaxConn = 100
                }
, limits = [ { lcPeriodSeconds = 7200, lcRequestsLimit = 820 }
           , { lcPeriodSeconds = 1, lcRequestsLimit = 9 }
           ]
}
