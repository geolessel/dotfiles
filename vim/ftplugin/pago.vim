"
" Pago
" screenwriting for vim
" Version:      0.2.31
" Updated:      2009-1-13
" Maintainer:   Mike Zazaian, mike@zop.io, http://zop.io
" License:      This file is placed in the public domain.
"
" Pago is the most powerful commandline screenwriting script available for any
" platform, and allows the use of vim as a fully-functional piece of screenwriting
" software, automatically formatting screenplay elements to the following
" specifications:
" 
" ELEMENT            ( beginning#, ending#, total#, align, caps )
"
" SCENE HEADING      (     11        70       60      L    yes  )
" ACTION             (     11        70       60      L     no  )
" CHARACTER          (     31        70       40      L    yes  )
" PARANTHETICAL      (     26        55       30      L     no  )
" DIALOGUE           (     21        55       35      L     no  )
" TRANSITION         (     70        11       60      R    yes  )
"
" This plugin was inspired by the screenplay.vim plugin developed by Alex Lance,
" which supported ACTION lines, CHARACTER names, and DIALOGUE.
"
"
"  Overview
"  ========
"
"  * Supports all major formatting elements of a screenplay, based on both
"    logical and commonly accepted conventions of the screenplay form:
"
"          INT. WAREHOUSE - DAY
"
"          Sunlight peaks through crumbled roof tiles.  NELSON, 40, ruggedlooks up from a
"          newspaper as a 
"
"  * Automatically enforces all boundaries within each screenplay element, ensuring
"    proper formatting to the exact specifications of a screenplay.
"
"  * Allows access to all six available screenplay ELEMENTS without any complex
"    keyboard shortcuts or commands, using only the keys <Backspace>, <Tab>, <Enter>,
"    <Up>, <Down>, <Left>, <Right>.
"
"  * Allows easy cycling through blank screenplay elements using both the <Tab> and
"    <Backspace> keys.
"
"
"  Features
"  ========
"
"  * Pressing TAB cycles through empty screenplay elements in the order:
"    
"    ACTION --> DIALOGUE --> PARENTHETICAL --> CHARACTER --> TRANSITION --> ACTION
"
"    * Pressing <Tab> from a blank TRANSITION element will cycle back to the
"      beginning of the line, triggering a blank ACTION element.
"    
"    * Parentheses are automatically inserted when calling a blank PARENTHETICAL
"      element.  Pressing <Tab> from within blank parentheses will automatically
"      delete the parentheses and jump to a blank CHARACTER element.
"
"    * A colon (":") is automatically inserted at the end of the line when an
"      TRANSITION element is called.  The cursor remains at the spot of the colon
"      when text is either entered or deleted.
"      
"      * Text typed within a TRANSITION element is automatically RIGHT-JUSTIFIED.
"
"      * Pressing <Enter> from a TRANSITION element jumps two lines down the page
"        and prompts a blank SCENE HEADING element.
"
"  * Pressing <Backspace> on an empty line cycles through empty screenplay elements
"    in the reverse order:
"
"    TRANSITION --> CHARACTER --> PARENTHETICAL --> DIALOGUE --> ACTION
"    --> END OF PREVIOUS ELEMENT
"
"    * Pressing <Backspace> from a blank line jumps to the end of the previous
"      element or, if the above two lines are blank, will create a blank ACTION
"      element two lines above the previous cursorline.
"    
"    * Pressing <Backspace> on a line with text will delete the character to the
"      left of the cursor.
"
"  * All text typed with a SCENE HEADING, CHARACTER, or TRANSITION element will be
"    automatically CAPITALIZED.
"
"  * To create a SCENE HEADING element, press enter while in a blank ACTION element.
"    The Cursorline will be HIGHLIGHTED and all text typed within the element will
"    be CAPITALIZED.
"
"  * While in a blank SCENE HEADING element:
"    * Press the <Space> bar to cycle through the common prefixes INT., EXT., and
"      INT./EXT.
"    * Press <Enter> to jump down two lines to a new, blank ACTION element.
"
"  * Active screenplay element is displayed in CAPS in the status bar.
"
"  * Page number is displayed in the status bar.  This estimates the number of pages
"    within your screeplay using a 56-line-per-page standard.
"
"  * Pressing <Up> in either INSERT or NORMAL modes jumps to the beginning of the
"    line above the cursorline.
"
"  * Pressing <Down> in either INSERT or NORMAL modes jumps to the end of the
"    line above the cursorline.
"
"  * Holding <Left> in either INSERT or NORMAL modes scrolls through to the
"    beginning of the current element, then jumps to the end of the previous
"    element.
"
"  * Holding <Right> in either INSERT or NORMAL modes scrolls through to the
"    end of the current element, then jumps to the beginning of the next
"    element.
"
"  * Typing text within a screenplay element such as DIALOGUE or ACTION will
"    automatically reformat the paragraph if text exceeds the preset end of the
"    line.  This improves upon use of the :tw (text width) and :wrap commands by
"    formatting text that is typed within a paragraph, rather than simply at the end
"    of it.
"
"
"  TODO
"  ====
"  - provide indentation and textwidth settings for ACTOR DIRECTIONS (31
"    spaces and then ACTOR DIRECTION in brackets)
"  - make vim style help for this plugin
"  - syntax highlighting
"
"  INSTALLATION
"  ============
"
"  * Drop this file in your ${VIMRUNTIME}/ftplugin/ directory, which is likely
"    located at ~/.vim/ftplugin/ 
"
"  * add the following lines to your ~/.vimrc file:
"    :filetype on
"    :filetype plugin on
"    :au BufRead,BufNewFile *.pago    set filetype=pago
"
"  * Ensure the suffix the file you are editing is .pago and away you
"    go!
"
"
" Avoid loading this twice
if exists("loaded_pago")
  finish
endif
let loaded_pago = 1
let g:counter = []

" Three listeners: Enter, Tab and Backspace
imap <CR> <C-R>=EnterPressed()<CR>
imap <TAB> <C-R>=TabPressed()<CR>
imap <BS> <C-R>=BackspacePressed()<CR><C-R>=ElementDetect()<CR>
imap  <C-R>=BackspacePressed()<CR><C-R>=ElementDetect()<CR>

ino <Up> <C-R>=DirectionPressed("up")<CR><C-R>=ElementDetect()<CR><C-R>=CursorAdjust("up")<CR>
ino <Down> <C-R>=DirectionPressed("down")<CR><C-R>=ElementDetect()<CR><C-R>=CursorAdjust("down")<CR>
ino <Left> <C-R>=DirectionPressed("left")<CR><C-R>=ElementDetect()<CR><C-R>=CursorAdjust("left")<CR>
ino <Right> <C-R>=DirectionPressed("right")<CR><C-R>=ElementDetect()<CR><C-R>=CursorAdjust("right")<CR>
ino <PageDown> <PageDown><C-R>=DirectionPressed("up")<CR><C-R>=ElementDetect()<CR><C-R>=CursorAdjust("up")<CR>
ino <PageUp> <PageUp><C-R>=DirectionPressed("down")<CR><C-R>=ElementDetect()<CR><C-R>=CursorAdjust("down")<CR>

no <Up> a<C-R>=DirectionPressed("up")<CR><C-R>=ElementDetect()<CR><C-R>=CursorAdjust("up")<CR><Esc>
no <Down> a<C-R>=DirectionPressed("down")<CR><C-R>=ElementDetect()<CR><C-R>=CursorAdjust("down")<CR><Esc>
no <Left> a<C-R>=DirectionPressed("left")<CR><C-R>=ElementDetect()<CR><C-R>=CursorAdjust("left")<CR><Esc>
no <Right> a<C-R>=DirectionPressed("right")<CR><C-R>=ElementDetect()<CR><C-R>=CursorAdjust("right")<CR><Esc>
no <PageDown> a<PageDown><C-R>=DirectionPressed("up")<CR><C-R>=ElementDetect()<CR><C-R>=CursorAdjust("up")<CR><Esc>
no <PageUp> a<PageUp><C-R>=DirectionPressed("down")<CR><C-R>=ElementDetect()<CR><C-R>=CursorAdjust("down")<CR><Esc>

ino <Space> <Space><C-R>=SceneStart()<CR><Esc>
no <Space> <Space><C-R>=SceneStart()<CR><Esc>

" Reformat paragraph with Ctrl-P in insert and normal mode
imap <C-P> <C-R>=CtrlPPressed()<CR>
map <C-P> i<C-R>=CtrlPPressed()<CR>

" User-Defined Commands
" :command -nargs=0 W "mz:w"
" :command -nargs=0 WQ "mz:wq"
" :command -nargs=0 WQQ "mz:wq!"


" map ctrl-d to clean up all the whitespace so that ctrl-p work correctly
"imap <C-D> !A!<Esc>:%s/^[ ]\{1,}$//g<CR>?!A!<CR>df!

setlocal tw=70         " Set text width to 70
setlocal wrap          " Set columns to wrap at tw
setlocal fo+=w
setlocal ls=2          " Always show statusline
setlocal expandtab     " Change tabs into spaces
setlocal softtabstop=0 " softtabstop variable can break my custom backspacing
setlocal autoindent    " Set auto indent
setlocal noshowmatch   " Turn off display of matching parenthesis if already on
setlocal ff=unix       " use unix fileformat
set ai                 " set autoindenting
set si                 " set smartindenting

fu! TransitionAdjust()
  if g:current == "transition"
     let rtn = "\<Esc>:s/^\ //\<CR>:let @/ =\"\"\<CR>A\<Left>"
"     let [lnum, s:whitespace] = searchpos("[A-Za-z_:]", "bnc" , line("."))
"     let s:movements = 70 - s:whitespace
"     let s:taprepend = repeat('\<Left\>', s:movements)
"     let s:tamiddle = "\<bs>"
"     let s:taappend = repeat('\<Right\>', s:movements)
"     let rtn = s:taprepend . s:tamiddle . s:taappend
  else
    let rtn = ""
  endif
  return rtn
endfu

" Dictionary Definitions
let g:alphalower = ['a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z']
let g:alphaupper = []
for n in g:alphalower
  exe "let N = toupper('" . n . "')"
  exe "let g:alphaupper += ['" . N . "']"
endfor
let g:alphaall = g:alphalower + g:alphaupper
let g:otherkeys = ['<Space>','!','.','-','?',';']


fu! ResetCursor(initline, initcol)
  
  exe "let thisends = g:" . g:current . ".ends"
  exe "let thisbegins = g:" . g:current . ".begins"

  let s:newcol = a:initcol + 1
  call cursor(a:initline, a:initcol)

  let s:endspace = search(" $","bnc") 
  
  if a:initcol >= thisends
    if s:endspace == a:initline 
      let s:newcol = thisbegins
      let s:newline = a:initline + 1
      let s:trail = "\<Space>\<Left>"
    else
      call cursor(a:initline, col("$"))
      let s:lastspace = search(" ","bc",line("."))
      let s:trail = "\<Del>\<CR>\<Right>"
      let s:newline = line(".")
      let s:newcol = col(".")
    endif
  else
    let s:newline = a:initline
    let s:trail = ""
  endif
  
  let s:rtn = ":call cursor(" . s:newline . "," . s:newcol . ")\<CR>" . s:trail
endfu


fu! Format()
  let s:initline = line(".")
  let s:initcol = col(".")
  let s:topline = s:initline
  let s:botline = s:initline
  
  while indent(s:topline) == indent(s:initline)
    let s:topline -= 1
  endwhile

  while indent(s:botline) == indent(s:initline)
    let s:botline += 1
  endwhile

  let s:topline +=1
  let s:botline -=1

  let s:lines = s:botline - s:topline
  exe "let currentend = g:" . g:current . ".ends"

  " let s:rtn = "\<Esc>:" . s:topline . "\<CR>v" . s:lines . "jgw:call ResetCursor(" . s:initline . "," . s:initcol . ")\<CR>i"
  let s:rtn = "\<Esc>gw}a"
  return s:rtn
endfu

  let g:autoformat = "<C-R>=Format()<CR>"

" Definition of Accepted Screenplay Characters
let g:screenchars = '[A-Za-z_0-9\?\!\.\-\(\)]'
let g:emptyline = "[^ ].*"

fu! MapUppercase()  
  if g:current == "transition"
    let g:premap = "<C-R>=TransitionAdjust()<CR>"
  else
    let g:premap = ""
  endif
  
  for n in g:alphaall
    exe "ino " . n . " " . g:premap . toupper(n) . g:autoformat
  endfor

  for key in g:otherkeys
    let key1 = key
    let key2 = key

    if g:current == "scene" && key == "<Space>"
      let g:premap = "<C-R>=SceneStart()<CR>"
      let key2 = ""
    elseif g:current == "transition"
      let g:premap = "<C-R>=TransitionAdjust()<CR>"
    else
      let g:premap = ""
    endif

    exe "ino " . key1 . " " . g:premap . key2 . g:autoformat
  endfor

  return ""
endfu

fu! UnmapUppercase()
  for n in g:alphaall
    exe "ino " . n . " " . n . g:autoformat
  endfor

  for n in g:otherkeys
    exe "ino " . n . " " . n . g:autoformat
  endfor

  return ""
endfu

function! ToggleCase(new_case)
  if a:new_case == "upper"
    call MapUppercase()
    return "gUgU"
  elseif a:new_case == "lower"
    call UnmapUppercase()
    return "gugu"
  endif
endfunction



" Line Length and Cursor Position Functions
fu! LineStart(line_num)
  let [s:lnum, s:linestart] = searchpos("[^ ].*", "bnc", line(a:line_num))
  return s:linestart
endfu

fu! LineEnd(line_num)
  let [s:lnum, s:endline] = searchpos("$", "nc", line(a:line_num))
  return s:endline
endfu

fu! ColonPos(line_num)
  let [s:lnum, s:colonpos] = searchpos(":", "nc", line(a:line_num))
  return s:colonpos
endfu

fu! ColonEnd(line_num)
  let s:colonend = search(":$", "nc", line(a:line_num))
  return s:colonend
endfu

fu! CursorPos(line_num)
    let [s:buffer, s:lnum, s:cursorpos, s:off] = getpos(a:line_num)
    return s:cursorpos
endfu

fu! LowerChars(line_num)
  let s:lowerchars = search("[a-z]", "nc", line(a:line_num))
  return s:lowerchars
endfu

fu! HasChars(line_num)
  let s:result = search("\([A-Z]\)\|\([a-z]\)\|\([A-Za-z_0-9]\)", "bncp", line(a:line_num))
  return s:result
endfu

fu! ScreenChars(line_num)
  let s:result = search(g:screenchars, "bncp", line(a:line_num))
  return s:result
endfu


fu! NewLineRange(line_num, direction)
  let s:thisline = line(a:line_num)

  if a:direction == "up" || a:direction == "left"
    let s:newline = s:thisline - 1
    let s:nextline = s:newline - 1
  elseif a:direction == "down" || a:direction == "right"
    let s:newline = s:thisline + 1
    let s:nextline = s:newline + 1
  endif

  let s:newlinestart = LineStart(s:newline)
  let s:newlineend = LineEnd(s:newline)
  let s:newindent = indent(s:newline)
  let s:newchars = ScreenChars(s:newline)
  return [s:newlinestart, s:newindent, s:newchars, s:newlineend, s:newline, s:nextline]
endfu

fu! ElementDetect()

  " Detect indent of new line
  let s:indent = indent(line("."))
  let s:colon = ColonPos(".")
  let s:colonend = ColonEnd(".")
  let s:lowerchars = LowerChars(".")
  let s:chars = LineStart(".")
  let s:x_coord = col(".")

  if s:colon != 70 && s:colonend < 1 
    
    if s:indent == 10
      " Check whether the line is a SCENE element
      let s:n = search('^[ ].*[INT|EXT]\.', 'bncp', line("."))
      " Check whether there are any lowercase characters on the line
      let s:l = LowerChars(".")
      if s:n > 0 && s:l < 1
        call Element(g:scene)
      else
        call Element(g:action)
      endif
    elseif s:indent == 20 
      call Element(g:dialogue)
    elseif s:indent == 25
      call Element(g:parenthetical)
    elseif s:indent == 30
      call Element(g:character)
    endif
  elseif s:colon == 70 || s:colonend > 0
    call Element(g:transition)
  else
    call Element(g:action)
  endif
  
  return '' 
endfu

" End Line Length and Cursor Position Functions

" If SCENE is the active element, cycle through scene prefixes with the <Space> bar
fu! SceneStart()
  if g:current == "scene"

    let s:scenelist = search('\(INT\.\ \)\|\(EXT\.\ \)\|\(INT\.\/EXT\.\ \)\|\([^EXT\. |INT\. |INT\.\/EXT\. ]\)', "bncpe", line("."))
    let s:lineend = LineEnd(".")
    
    if s:scenelist != 5
      let s:clearsearch = ":let @/ = \"\"\<CR>"
      if s:scenelist == 0
        let s:rtn = "INT. "
      elseif s:scenelist == 2
        let s:rtn = "\<Esc>:s/INT\\. .*/EXT\\. /\<CR>" . s:clearsearch . "A"
      elseif s:scenelist == 3
        let s:rtn = "\<Esc>:s/EXT\\. .*/INT\\.\\/EXT\\. /\<CR>" . s:clearsearch . "A"
      elseif s:scenelist == 4
        let s:rtn = "\<Esc>:s/INT\\.\\/EXT\. .*/INT\\. /\<CR>" . s:clearsearch . "A"
      endif

    else
      let s:rtn = "\<Space>"
    endif
  
  else
    let s:rtn = "\<Space>"
  endif

  return s:rtn
endfu


let g:scene = { 'name': 'scene', 'begins': 11, 'ends': 70, 'case': 'upper', 'align': 'L' }
let g:action = { 'name': 'action', 'begins': 11, 'ends': 70, 'case': 'lower', 'align': 'L' }
let g:dialogue = { 'name': 'dialogue', 'begins': 21, 'ends': 55, 'case': 'lower', 'align': 'L' }
let g:parenthetical = { 'name': 'parenthetical', 'begins': 26, 'ends': 55, 'case': 'lower', 'align': 'L' }
let g:character = { 'name': 'character', 'begins': 31, 'ends': 70, 'case': 'upper', 'align': 'L' }
let g:transition = { 'name': 'transition', 'begins': 70, 'ends': 11, 'case': 'upper', 'align': 'R' }

fu! Element(element)
  let g:previous = g:current
  let g:current = a:element.name

  if g:current == "scene"
    set cursorline
  else
    set nocursorline
  endif

  if g:current == "transition"
    let s:textwrap = 1000
  else
    let s:textwrap = a:element.ends
  endif

  exe "set tw=" . s:textwrap
  call ToggleCase(a:element.case)

  " Page Number
  let pageInt = (line(".") / 52) + 1
  let g:page = "PAGE " . pageInt
  
  let g:statustxt = toupper(g:current)
  set statusline=%<[%02n]\ %F%(\ %m%h%w%y%r%)\ %{g:page}\ \-\ %{g:statustxt}\ %a%=\ %8l,%c%V/%L\ (%P)\ [%08O:%02B]

  return ''
endfu

fu! EnterPressed()
  let s:linestart = LineStart(".")
  let s:lineend = LineEnd(".")
  let s:col = col(".")
  
  if g:current == "scene" || g:current == "action"
    call Element(g:action)
    if s:linestart > 0
      let s:rtn = "\<CR>\<CR>"
    else
      if exists("g:switch")
        let g:switch += 1
      else
        let g:switch = 1
      endif
      let s:rtn = ""
      if g:previous == "action"
        call Element(g:scene)
        let g:switch = 1
      endif
      if g:switch == 2
        let s:rtn = "\<CR>\<CR>"
        let g:switch = 0
      endif
    endif

  elseif g:current == "dialogue"
    call Element(g:parenthetical)
    let s:rtn = "\<CR>\<Esc>I".repeat(' ', g:parenthetical.begins - 1)."\(\)\<left>"
    let g:parensfromdialogue = 1

  elseif g:current == "parenthetical"
    call cursor(line("."), s:lineend)
    let s:emptyparens = search("()", "bnc", line("."))

    if g:parensfromdialogue == 1 && s:emptyparens != 0
      call Element(g:character)      
      let s:rtn = repeat("\<BS>", s:col)."\<CR>\<Esc>I".repeat(' ', g:character.begins - 1)
    else
      call Element(g:dialogue)
      let s:rtn = "\<CR>\<Esc>I".repeat(' ', g:dialogue.begins - 1)
    endif
    g:parensfromdialogue = 0

  elseif g:current == "character"
    if s:linestart < 1
      let s:rtn = repeat("\<BS>", s:col - g:action.begins)
      call Element(g:action)
    else
      let s:rtn = "\<CR>\<Esc>I".repeat(' ', g:dialogue.begins - 1)
      call Element(g:dialogue)
    endif

  elseif g:current == "transition"
    call Element(g:scene)
    call cursor(line("."), s:lineend)
    let screenchars = ScreenChars(".")
    if screenchars == 0
      let s:append = "\<Left>\<Del>"
    else
      let s:append = ""
    endif
    let s:rtn = s:append . "\<CR>\<CR>\<Esc>i" . repeat(" ", g:action.begins - 1)
  endif

  return s:rtn
endfu


function! TabPressed()
  let s:col = col(".")
  
    if g:current == "action" || g:current == "scene"
      let e = g:dialogue
      let s:x_change = e.begins - s:col
      let s:rtn = repeat(' ', s:x_change)

    elseif g:current == "dialogue"
      let e = g:parenthetical
      let s:x_change = e.begins - s:col
      let s:rtn = repeat(' ', s:x_change) . "()\<Left>"
    
    elseif g:current == "parenthetical"
      let e = g:character
      let s:x_change = e.begins - s:col
      let s:rtn = "\<Left>\<Del>\<Del>" . repeat(' ', s:x_change + 1)
    
    elseif g:current == "character"
      let e = g:transition
      let s:x_change = e.begins - s:col
      let s:rtn = repeat(' ', s:x_change) . ":\<Left>"
    
    elseif g:current == "transition"
      let e = g:action
      let s:rtn = "\<Del>" . repeat("\<BS>", s:col - 1) . repeat(' ', e.begins - 1)
    endif

  call Element(e)
  return s:rtn
endfunction


fu! BackspacePressed()
  let s:linestart = LineStart(".")
  let s:lineend = LineEnd(".")
  let s:screenchars = ScreenChars(".")
  let s:indent = indent(".")
  let s:col = col(".")
  let [s:newlinestart, s:newindent, s:newchars, s:newlineend, s:newline, s:nextline] = NewLineRange(".", "up")
  exe "let currentstart = g:" . g:current . ".begins"

  let s:rtn = "\<BS>"

  if s:screenchars == 0 || ( s:col <= currentstart + 1 && g:current != "transition" )
    if g:current == "transition"
      let s:rtn = "\<Del>" . repeat("\<BS>", s:col - g:character.begins)
  
    elseif g:current == "character"    
      let s:rtn = repeat("\<BS>", 5) . "()\<left>"
  
    elseif g:current == "parenthetical"
      let [s:lnum, s:openparen] = searchpos("(", "nc", line("."))
      call cursor(line("."), s:openparen)
      let s:rtn = "\<Del>\<Del>" . repeat("\<BS>", s:col - g:dialogue.begins)
  
    elseif g:current == "dialogue"
      if s:newindent < g:dialogue.begins - 1
        let s:rtn = repeat("\<BS>", 10)
      else
        let s:rtn = "\<BS>\<C-R>=DirectionPressed(\"left\")\<CR>\<C-R>=ElementDetect()\<CR>\<C-R>=CursorAdjust(\"left\")\<CR>"
      endif

    elseif g:current == "action" || g:current == "scene"
      let backspaces = s:col
      let s:nextindent = indent(s:nextline)
      if s:newindent < g:action.begins - 1
        let backspaces += 1
        if s:nextindent < g:action.begins - 1
          let s:trail = repeat(' ', g:action.begins - 1) 
        else
          let s:trail = ""
        endif
      else
        let s:trail = ""
      endif
      let s:rtn = repeat("\<BS>", backspaces) . s:trail . "\<Esc>gw}a"
  
    endif
  elseif g:current == "transition"
    let s:rtn = "\<BS>\<Esc>:s/^/ /\<CR>:let @/ =\"\"\<CR>A\<Left>"
  endif
 
  return s:rtn
endfu

fu! DirectionPressed(dir)
  exe "let key = \"\\\<" . a:dir . ">\""
  exe "let thisbegins = g:" . g:current . ".begins"
  exe "let thisends = g:" . g:current . ".ends"
  let lineend = LineEnd(".")
  let thisline = line(".")
  let thisindent = indent(".")
  let col = col(".")
  let [s:newlinestart, s:newindent, s:newchars, s:newlineend, s:newline, s:nextline] = NewLineRange(".", a:dir)
  let moves = 1
  let g:linejump = "yes"

  if s:newindent < g:action.begins - 1
    let moves += 1
  endif

  if a:dir == "left"
    if col <= thisindent + 1
      let key = "\<Up>"
    else
      let moves = 1
      let g:linejump = "no"
    endif

  elseif a:dir == "right"
    if col >= lineend
      let key = "\<Down>"
    else
      let moves = 1
      let g:linejump = "no"
    endif

  endif

  return repeat(key, moves)
endfu

fu! CursorAdjust(dir)
  let s:lineend = LineEnd(".")
  let s:col = col(".")
  exe "let thisbegins = g:" . g:current . ".begins"
  let s:rtn = ""

  if g:linejump == "yes"
    if a:dir == "up" || a:dir == "left"
      if s:col < s:lineend
        let s:rtn = repeat("\<Right>", s:lineend - s:col)
      endif
  
    elseif a:dir == "down" || a:dir == "right"  
      if s:col > thisbegins
        let s:rtn = repeat("\<Left>", s:col - thisbegins)
      else
        let s:rtn = repeat("\<Right>", thisbegins - s:col)
      endif
  
    endif
  endif

  return s:rtn
endfu


function! CtrlPPressed()
  let s:linestart = LineStart(".")
  if s:linestart == 31
    set tw=55
    return "\<Esc>gq}i"
  elseif s:linestart == 11
    set tw=70
    return "\<Esc>gq}i"
  endif
  return "\<Esc>gq}i"
endfunction


" This function allows a dropdown list to 
" appear for character names at the top of DIALOG
function! CompleteCharacterName(findstart, base)
  if a:findstart
    " locate the start of the word
    let line = getline('.')
    let start = col('.') - 1
    while start > 0 && line[start - 1] =~ '\a'
      let start -= 1
    endwhile
    return start
  else
    let last_line = str2nr(line("$"))
    let line_num = 1
    let pattern = "^".repeat(" ",30)."[A-Za-z0-9 ']*$"
    "let pattern = 'combination'
    let matches = {}
    let names = []
    let res = []
    
    " need the call to cursos to start the search from the start of doc
    call cursor(1, 1) 

    " loop through all the line
    while line_num <= last_line
      if search(pattern,"cn",line_num) > 0
        let k = substitute(getline(line_num),"^[ ]*","","")
        let k = substitute(k,"[ ]*$","","")
        if !has_key(matches,k) && strlen(k) > 0
          let matches[k] = 1
        endif
      endif
      let line_num = line_num + 1
      call cursor(line_num, 1) 
    endwhile
   
    for key in sort(keys(matches))
      call add(names, key)
    endfor

    for n in names
      if n =~ '^' . a:base
        call add(res, n)
      endif
    endfor
    return res
  endif
endfunction
set completefunc=CompleteCharacterName

fu! Start()
  let g:current = "action"

  if line("$") == 1 && indent(".") == 0
    call Element(g:action)
   exe "normal :s/^/           /g\<CR>:\<BS>"
  endif
endfu

"  let s:lastline = line(".")
"  let s:emptylines = []

"  for i in range(1, s:lastline)
"    if indent(i) == 0
"      let s:emptylines = s:emptylines + [i]
"    endif
"  endfor

"  for i in s:emptylines
"    call substitute(i, "^", "          ", "g")
"  endfor

call Start()
