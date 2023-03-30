refreshFrequency: 1

DARK = false

IDIOM = "<span>生きた轍を君と呼べ</span>"
IDIOM = "<span><a href='https:\/\/music.apple.com\/cn\/album\/%E6%82%B2%E3%81%97%E3%81%BF%E4%B8%80%E3%81%A4%E3%82%82%E6%AE%8B%E3%81%95%E3%81%AA%E3%81%84%E3%81%A7\/1538105636?i=1538105643'>悲しみ一つも残さないで</a></span>"
IDIOM = "<span>悲しみ一つも残さないで</span>"

commands =
  battery: "pmset -g batt | egrep '([0-9]+\%).*' -o --colour=auto " +
            "| cut -f1 -d';'"
  charging: "pmset -g batt | grep -c 'AC'"
  # time   : "date +\"%H:%M:%S\""
  # time   : "date +\"%H:%M\""
  # wifi   : "/System/Library/PrivateFrameworks/Apple80211.framework/" +
  #           "Versions/Current/Resources/airport -I | " +
  #           "sed -e \"s/^ *SSID: //p\" -e d"
  wifi   : "echo 0"
  music  : "echo 'tell application \"Music\"\nreturn name of current track\nend tell' | osascript"
  date   : "date +\"%a %d %b\""
  volume : "osascript -e 'output volume of (get volume settings)'"
  input : "osascript -e 'input volume of (get volume settings)'"
  cpu    : "ESC=`printf \"\e\"`; ps -A -r -o %cpu | awk '{s+=$1} END {printf(\"%2d\",s/8);}'"
  freemem: "memory_pressure | awk '/System-wide memory free percentage/ {print $5}' | sed 's/%//'"
  disk   : "df -H -l /System/Volumes/Data | awk '/\\/.*/ { print $5 }'"

colors =
  black   : "#3B4252"
  white   : "#D8DEE9"
  gray    : "#5C5C5C"
  red     : "#BF616A"
  green   : "#A3BE8C"
  dgreen  : "#4B7A47"
  yellow  : "#EBCB8B"
  dyellow : "#FFCC0D"
  blue    : "#4078c0"
  magenta : "#B48EAD"
  cyan    : "#88C0D0"
  acqua   : "#00d787"
  wine    : "#72003e"
  orange  : "#ff8700"

if DARK
    widgetc = 
      date : "#{colors.magenta}"
      input : "#{colors.yellow}"
      volume : "#{colors.yellow}"
      spacer : "#{colors.grey}"
      battery : "#{colors.white}"
      disk : "#{colors.green}"
      cpumem : "#{colors.cyan}"
      wifi : "#{colors.white}"
else
    widgetc = 
      date : "#{colors.yellow}"
      input : "#{colors.red}"
      volume : "#{colors.red}"
      spacer : "#{colors.grey}"
      battery : "#{colors.black}"
      disk : "#{colors.dgreen}"
      cpumem : "#{colors.blue}"
      wifi : "#{colors.black}"

command: "echo " +
          "$(#{commands.battery}):::" +
          "$(#{commands.charging}):::" +
          "$(#{commands.freemem}):::" +
          "$(#{commands.wifi}):::" +
          "$(#{commands.volume}):::" +
          "$(#{commands.input}):::" +
          "$(#{commands.date}):::" +
          "$(#{commands.cpu}):::" +
          "$(#{commands.disk}):::"
          # "$(#{commands.music}):::"

render: () ->
  """
  <link rel="stylesheet" href="./polybar/assets/font-awesome/css/all.css" />
  <div class="elements">
    <div class="idiom">#{IDIOM}</div>
    <div><span class="spacer">|</span></div>
    <div class="input">
      <span>
        <span class="mic-icon"></span>
        <span class="volume-input"></span>
      </span>
    </div>
    <div><span class="spacer">|</span></div>
    <div class="volume">
      <span>
        <span class="volume-icon"></span>
        <span class="volume-output"></span>
      </span>
    </div>
    <div><span class="spacer">|</span></div>
    <div class="cpumem">
      <span>
        <i class="fa fa-microchip"></i>
        <span class="cpu-output"></span>
        <span class="mem-output"></span>
      </span>
    </div>
    <div><span class="spacer">|</span></div>
    <div class="disk">
      <span>
        <i class="fa fa-hdd"></i>
        <span class="disk-output"></span>
      </span>
    </div>
    <!-- <div><span class="spacer">|</span></div> -->
    <!-- <div class="wifi"> -->
    <!--   <span> -->
    <!--     <i class="fa fa-wifi"></i> -->
    <!--     <span class="wifi-output"></span> -->
    <!--   </span> -->
    <!-- </div> -->
    <div><span class="spacer">|</span></div>
    <div class="battery">
      <span>
        <span class="battery-icon"></span>
        <span class="battery-output"></span>
      </span>
    </div>
    <div><span class="spacer">|</span></div>
    <div class="date">
      <span>
        <i class="fa fa-calendar"></i>
        <span class="date-output"></span>
      </span>
    </div>
    <!-- <div><span class="spacer">|</span></div> -->
    <!-- <div class="time"> -->
    <!--   <span> -->
    <!--     <i class="fa fa-clock"></i> -->
    <!--     <span class="time-output"></span> -->
    <!--   </span> -->
    <!-- </div> -->
  </div>
  """

update: (output) ->

  #console.log(output)
  output = output.split( /:::/g )

  # widgetc.battery = switch
    # when percentage <=  10 then "#{colors.red}"
    # when percentage <=  25 then "#{colors.orange}"
    # when percentage <=  50 then "#{colors.yellow}"
    # when percentage <=  75 then "#{colors.green}"
    # when percentage <= 100 then "#{colors.dgreen}"
  battery  = output[0]
  charging = output[1]
  mem      = 100 - Number(output[2])
  wifi     = output[3]
  volume   = output[4]
  input    = output[5]
  date     = output[6]
  cpu      = output[7]
  disk     = output[8]

  # $(".battery-output") .text("#{battery}")
  $(".mem-output")    .text("#{mem}%")
  # $(".wifi-output")    .text("#{wifi}")
  $(".volume-output")  .text("#{volume}%")
  $(".volume-input")  .text("#{input}%")
  $(".date-output")    .text("#{date}")
  $(".cpu-output")     .text("#{cpu}%")
  $(".disk-output")    .text("#{disk}")

  @handleBattery(Number(battery.replace( /%/g, "")), charging == '1')
  @handleVolume(Number(volume))
  @handleInput(Number(input))

handleBattery: ( percentage, charging ) ->
  if charging
    $(".battery-icon").html("<i class=\"fas fa-bolt \"></i>")
    return

  batteryIcon = switch
    when percentage <=  10 then "fa-battery-empty"
    when percentage <=  25 then "fa-battery-quarter"
    when percentage <=  50 then "fa-battery-half"
    when percentage <=  75 then "fa-battery-three-quarters"
    when percentage <= 100 then "fa-battery-full"
  
  $(".battery-icon").html("<i class=\"fa #{batteryIcon} \"></i>")

handleVolume: (volume) ->
  volumeIcon = switch
    when volume ==   0 then "fa-volume-off"
    when volume <=  50 then "fa-volume-down"
    when volume <= 100 then "fa-volume-up"
  $(".volume-icon").html("<i class=\"fa #{volumeIcon}\"></i>")

handleInput: (volume) ->
  micIcon = switch
    when volume ==   0 then "fa-microphone-slash"
    when volume <= 100 then "fa-microphone"
  $(".mic-icon").html("<i class=\"fa #{micIcon}\"></i>")


style: """

  .elements
    display: flex
    align-items: stretch
    height: 24px
    margin: 0 0px

  .elements > div
    display: flex
    align-items: center
    padding: 2px 2px
    margin: 0px auto

  a:link
    color: inherit !important

  .idiom > span
    padding: 3px 3px
    
  .spacer
    color: #{widgetc.spacer}

  .battery
    color: #{widgetc.battery}
  .wifi
    color: #{widgetc.wifi}
  .date
    color: #{widgetc.date}
  .cpumem
    color: #{widgetc.cpumem}
  .volume
    color: #{widgetc.volume}
  .input
    color: #{widgetc.input}
  .disk
    color: #{widgetc.disk}

  top: 5px
  right: 12px
  font-family: 'Monaco'
  font-size: 14px
  font-smoothing: antialiasing
  z-index: 0
"""
