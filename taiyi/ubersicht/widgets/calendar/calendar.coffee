#loosely based on https://github.com/cbeardsmore/Upcoming-iCal-Events/blob/master/Upcoming-iCal-Events.widget/main.coffee

# --------------- CUSTOMIZE ME ---------------
# these are dimensions of the widget in pixels
TOP = 38
LEFT = 1
BOTTOM = 3
WIDTH = 72
# font size of the event
EVENT_FONT_SIZE = 12

# how many pixels one-hour event occupies vertically
BLOCK_HEIGHT = 75
# place of first hour line
MIN_HOUR_LINE_TOP = 10
# event overlap indent
EVENT_OVLP_INDENT = 10
# left padding of dot
DOT_LEFT_PAD = 16
EVENT_RIGHT_PAD = 26
# automatic refresh per second
refreshFrequency: 0.1
# --------------------------------------------

# Construct bash command using options.
# Refer to https://hasseg.org/icalBuddy/man.html
executablePath = "/opt/homebrew/bin/icalBuddy "
baseCommand = ' eventsToday'
options = "-ea -npn -nrd -nc -b '' -nnr ' ' -iep 'title,datetime,notes' -ps '||' -po 'datetime,title,notes' -tf '%H:%M' -df '%Y-%m-%d'"

command: executablePath + options + baseCommand


style: """
    top: #{TOP}px
    left: #{LEFT}px
    bottom: #{BOTTOM}px
    width: #{WIDTH}px
    color: black
    font-family: Helvetica
    background-color: #ffffff;
    // border-width: 0.5px;
    // border: solid;
    border-color: #555555;
    overflow: hidden;

    z-index: 0

    div
        display: block
        color white
        font-size: 14px
        font-weight: 450
        text-align left
    #MIN_HOUR_LINE_TOP
        font-weight: bold
        font-size 20px
    .subhead
        font-weight: bold
        font-size: #{EVENT_FONT_SIZE}px
        border-left: solid 4px
        border-radius: 2px
        border-color: #bbbbbb
        padding-top 6px
        padding-bottom 3px
        padding-left: 5px
        background-color: #eeeeee

        mix-blend-mode: multiply;

        left: #{DOT_LEFT_PAD} px

        color: #000000
        overflow: hidden

    .subhead:hover
        background-color: #f0f8d0
        cursor: pointer
        cursor: hand

    #line
        left: 16px;
        width: #{WIDTH-EVENT_RIGHT_PAD+5}px;
        border: 1px solid #3e5ea3
    #hour-line
        left: 16px;
        width: #{WIDTH-EVENT_RIGHT_PAD+6}px;
        border: 0.5px solid #bbbbbb

    #line-text
        font-size: 12px
        left: 0px
        color: #3e5ea3
        background-color: #ffffff

    #hour-text
        font-size: 12px
        left: 0px
        color: #bbbbbb

    #dot
        left: #{DOT_LEFT_PAD-2} px;
        height: 8px;
        width: 8px;
        background-color: #3e5ea3;
        border-radius: 50%;
        display: inline-block;
"""

render: (output) -> """
"""

hours: (str) ->
    regex = /(\d+):(\d+)/
    result = regex.exec(str)
    return parseInt(result[1]) + parseInt(result[2])/60


intersects: (ev1, ev2) ->
    if ev1.start_time >= ev2.end_time or ev1.end_time <= ev2.start_time
        return false

    return true

afterRender: (domEl) ->
  $(domEl).on 'click', '.subhead', (e) =>
    link = $(e.currentTarget).attr 'data-link'
    @run "open " + link

update: (output, domEl) ->
    lines = output.split('\n')
    lines = lines.filter (line) -> line isnt ""

    dom = $(domEl)
    dom.empty()

    dom.append("""<link rel="stylesheet" href="./calendar/assets/font-awesome/css/all.css" />
""")

    today = new Date()
    # current_time = today.getHours().toString().padStart(2, '0') + ":" + today.getMinutes().toString().padStart(2, '0')
    # remove pad, to be consistent with date command line output
    current_min = today.getMinutes().toString().padStart(2, '0')
    current_time = today.getHours().toString() + ":" + current_min

    current_hour =  today.getHours() + today.getMinutes() / 60
    min_hour = current_hour

    line_regex = /^(\d+-\d+-\d+)?(?: at )?((?:\d+:\d+)|(?:\.\.\.)) - (\d+-\d+-\d+)?(?: at )?((?:\d+:\d+)|(?:\.\.\.))([^]*)?([^]*)?$/
    link_regex = /((https:\/\/.*zoom\.us\/j\/[^ ]*)|(https:\/\/meeting\.tencent\.com\/dm\/[^ ]*))/

    for line in lines
        console.log(line)
        start_time = line_regex.exec(line)[2]
        if start_time == '...'
            start_time = "00:00"
        start_hour = @hours(start_time)
        min_hour = Math.min(min_hour, start_hour)

    min_hour = Math.floor(min_hour) - 0.2

    events = []

    for line in lines
        result = line_regex.exec(line)
        if result[2] == '...'
            result[2] = "00:00"
        if result[4] == '...'
            result[4] = "24:00"

        link = link_regex.exec(result[6])

        event =
            start_date: result[1]
            start_time: @hours(result[2])
            end_date: result[3] or result[1]
            end_time: @hours(result[4])
            title: result[5]
            notes: result[6]
            link: link or ""

        events.push(event)

    for event in events
        diff_hours = (event.end_time - event.start_time)
        rel_start_hour = event.start_time - min_hour

        start_pos = rel_start_hour * BLOCK_HEIGHT
        height = diff_hours * BLOCK_HEIGHT

        left = DOT_LEFT_PAD
        wdth = WIDTH - EVENT_RIGHT_PAD - 2;

        for ev in events
            if @intersects(event, ev)
                if Math.abs(event.start_time - ev.start_time) < 0.5
                    if event.start_time < ev.start_time
                        wdth = (WIDTH-EVENT_RIGHT_PAD) / 2
                    if event.start_time == ev.start_time and event.end_time > ev.end_time
                        wdth = (WIDTH-EVENT_RIGHT_PAD) / 2
                    if event.start_time == ev.start_time and event.end_time == ev.end_time and event.title != ev.title
                        if event.title < ev.title
                            wdth = (WIDTH-EVENT_RIGHT_PAD) / 2
                        else
                            wdth = (WIDTH-EVENT_RIGHT_PAD) / 2 - EVENT_OVLP_INDENT
                            left = DOT_LEFT_PAD + (WIDTH-EVENT_RIGHT_PAD) / 2 + EVENT_OVLP_INDENT

                    if event.start_time > ev.start_time or event.end_time < ev.end_time
                        wdth = (WIDTH-EVENT_RIGHT_PAD) / 2 - EVENT_OVLP_INDENT
                        left = DOT_LEFT_PAD + (WIDTH-EVENT_RIGHT_PAD) / 2 + EVENT_OVLP_INDENT
                else
                    if event.start_time > ev.start_time
                        left = DOT_LEFT_PAD + 5
                        wdth = WIDTH - EVENT_RIGHT_PAD - 5

        video_icon = ""

        if event.link != ""
            video_icon = """<i class="fa fa-video" style="font-size: 8px"></i>"""

        str = """<div class="subhead" data-link="#{event.link}" style="position: absolute; top: #{start_pos+MIN_HOUR_LINE_TOP+10}px; height: #{height-10}px; width: #{wdth}px; left: #{left}px;">
            #{video_icon}
            #{event.title}
            </div> """

        dom.append(str)

    rel_current_hour = current_hour - min_hour
    # dom.append("""<span id="line-text" style="position: absolute; top: #{rel_current_hour * BLOCK_HEIGHT + MIN_HOUR_LINE_TOP + 2}px;">#{current_time}</span>""")
    dom.append("""<span id="line-text" style="position: absolute; top: #{rel_current_hour * BLOCK_HEIGHT + MIN_HOUR_LINE_TOP + 2}px;">#{current_min}</span>""")

    for hour in [Math.floor(min_hour+1)...25]
        rel_hour = hour - min_hour
        hour_show = hour.toString().padStart(2, '0')

        dom.append("""<hr id="hour-line" style="position: absolute; top: #{rel_hour * BLOCK_HEIGHT + MIN_HOUR_LINE_TOP}px;"></hr>""")
        # dom.append("""<span id="hour-text" style="position: absolute; top: #{rel_hour * BLOCK_HEIGHT + MIN_HOUR_LINE_TOP + 2}px;">#{hour}:00</span>""")
        dom.append("""<span id="hour-text" style="position: absolute; top: #{rel_hour * BLOCK_HEIGHT + MIN_HOUR_LINE_TOP + 2}px;">#{hour_show}</span>""")

    dom.append("""<hr id="line" style="position: absolute; top: #{rel_current_hour * BLOCK_HEIGHT + MIN_HOUR_LINE_TOP}px;"></hr>""")
    dom.append("""<span id="dot" style="position: absolute; top: #{rel_current_hour * BLOCK_HEIGHT + MIN_HOUR_LINE_TOP + 5}px;"></span>""")
