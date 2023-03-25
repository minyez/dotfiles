refreshFrequency: false

DARK = false
if DARK
    bgcolor = "#1C2331"
else
    bgcolor = "#FFFFFF"

render: (output) ->
  ""

style: """
    top: 0px
    left: 0px;
    right: 0px;

    height: 32px
    background-color: #{bgcolor}
    //opacity: 0.125
    z-index: -1
"""
