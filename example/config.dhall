{
  -- disambiguation configuration
  disambCfg =
    { tiersCfg =
    	[ {withPos=True, withEos=False, withAtts=
            ["cas", "per"]
          }
    	, {withPos=False, withEos=False, withAtts=
            [ "nmb", "gnd", "deg", "asp" , "ngt", "acm"
            , "acn", "ppr", "agg", "vlc", "dot"
            , "sbg", "col"
            ]
          }
        ]
    }
}
