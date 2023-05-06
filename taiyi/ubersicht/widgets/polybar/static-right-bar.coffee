refreshFrequency: false

DARK = false

IDIOM = "<span>修</span>"

colors =
  black   : "#3B4252"
  white   : "#D8DEE9"

if DARK
    widgetc = 
      text : "#{colors.white}"
else
    widgetc = 
      text : "#{colors.black}"

command: "echo 1"

render: () ->
  """
  <link rel="stylesheet" href="./polybar/assets/font-awesome/css/all.css" />
  <div class="elements">
    <div class="idiom">#{IDIOM}</div>
  </div>
  """

update: (output) ->

  # console.log(output)


style: """

  .elements
    display: flex
    align-items: stretch
    height: 24px
    margin: 0 10px
    cursor: default // selectable text, but cursor will not change to I

  .elements > div
    display: flex
    align-items: center
    padding: 2px 2px
    margin: 0px auto

  a:link
    color: inherit !important

  .idiom > span
    padding: 3px 3px

  top: 5px
  right: 12px
  font-family: 'Monaco'
  font-size: 14px
  font-smoothing: antialiasing
  z-index: 0
"""
