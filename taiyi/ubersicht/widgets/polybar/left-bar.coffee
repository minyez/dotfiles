refreshFrequency: false

DARK = false

commands =
  active : "/opt/homebrew/bin/yabai -m query --spaces --space 2>/dev/null | /opt/homebrew/bin/jq '.index'"
  list   : "/opt/homebrew/bin/yabai -m query --spaces 2>/dev/null | /opt/homebrew/bin/jq -r '.[].label'"
  monitor: ""

cdef =
  acqua:   "#00d787"
  wine:    "#72003e"
  orange:  "#ff8700"
  ghblue:  "#4078c0"
  silver:  "#e4e4e4"
  elegant: "#1C2331"
  magenta: "#af005f"
  cyan:    "#00afd7"
  white:   "#FFFFFF"


if DARK
  colors = 
    workspace_fg : "#{cdef.orange}"
    workspace_bg : "#{cdef.elegant}"
    active_fg : "#{cdef.elegant}"
    active_bg : "#{cdef.silver}"
else
  colors = 
    workspace_fg : "#{cdef.ghblue}"
    workspace_bg : "#{cdef.white}"
    active_fg : "#{cdef.elegant}"
    active_bg : "#{cdef.silver}"

command: "echo " +
          "$(#{commands.active}):::" +
          "$(#{commands.list}):::"

render: () ->
  """
  <link rel="stylesheet" href="./polybar/assets/font-awesome/css/all.css" />
  <div class="spaces">
    <div>1: Default</div>
  </div>
  """

update: (output) ->
  output = output.split( /:::/g )

  active = output[0]
  list   = output[1]

  @handleSpaces(list)
  @handleActiveSpace(Number (active))

handleSpaces: (list) ->
  $(".spaces").empty()
  list = " " + list
  list = list.split(" ")

  # $(".spaces").append("""<div class="workspace fab fa-apple"></div>""")
  $.each(list, (index, value) ->
    if (index > 0)
      $(".spaces").append(
         """<div class="workspace" id="#{index}">#{index}:#{value}</div>"""
      )
      #$("<div>").prop("id", index).text("#{index}: #{value}").appendTo(".spaces")
  )

handleActiveSpace: (id) ->
  $("##{id}").addClass("active")

style: """
  .spaces
    display: flex
    align-items: stretch
    height: 24px

  .workspace
    display: flex
    color: #{colors.workspace_fg}
    align-items: center
    justify-content: center
    padding: 3px 3px
    border: 1px solid #{colors.workspace_bg}

  .active
    color: #{colors.active_fg}
    background: #{colors.active_bg}
    border: 1px solid #{colors.active_fg}

  top: 5px
  left: 6px
  font-family: 'Monaco'
  font-size: 14px
  font-smoothing: antialiasing
  z-index: 0
"""
