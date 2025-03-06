type Mode =
  | Nocturne // like normal
  | Incantation // like insert
  | Conjuration // like command

type CursorShape =
  | None = -1
  | BlinkingHalfBlock = 0
  | BlinkingBlock = 1
  | SteadyBlock = 2
  | BlinkingUnderline = 3
  | SteadyUnderline = 4
  | BlinkingBar = 5
  | SteadyBar = 6

type Line = { Raw: string; Chars: string [] }

type Status = { Message: string option; Timestamp: System.DateTime option }

type EditorConfig = {
  CaretX: int
  CaretY: int
  RenderX: int
  RowOffset: int
  ColumnOffset: int
  ScreenRows: int
  ScreenColumns: int
  Lines: Line []
  TabStop: int
  IsDirty: bool
  Filename: string option
  Status: Status
  CursorShape: CursorShape
  Mode: Mode
  IsQuitting: bool
}

let setStatusMessage message editor =
  { editor with Status = { Message = Some message; Timestamp = Some System.DateTime.Now } }


module Debug =
  type LogLevel = Info | Debug | Error | Warn

  let private path = "debug.log"

  let private levelToStr level =
    match level with
    | Info -> "INFO"
    | Debug -> "DEBUG"
    | Error -> "ERROR"
    | Warn -> "WARN"

  let private formatLog level message =
    let timestamp = System.DateTime.UtcNow.ToString "yyyy-MM-dd HH:mm:ss.fff"
    let level = level |> levelToStr
    sprintf "[%s] [%s] %s" timestamp level message

  let writeLog level message =
    use sw = path |> System.IO.File.AppendText
    formatLog level message
    |> sw.WriteLine

  let sleep (ms: int) =
    async { do! Async.Sleep ms } |> Async.RunSynchronously


module Terminal =
  let enableAlternativeBuffer (buffer: System.Text.StringBuilder) =
    "\u001b[?1049h" |> buffer.Append

  let disableAlternativeBuffer (buffer: System.Text.StringBuilder) =
    "\u001b[2;1H\u001b[?1049l" |> buffer.Append


module Color =
  type ColorType = Foreground | Background
  module BasicColor =
    type private BaseColor =
      | Black = 0 | Red = 1 | Green = 2 | Yellow = 3
      | Blue = 4 | Magenta = 5 | Cyan = 6 | White = 7

    let private baseToAnsi baseColor colorType isBright =
      let code =
        match colorType with
        | Foreground -> if isBright then 90 else 30
        | Background -> if isBright then 100 else 40
      sprintf "\u001b[%dm" (baseColor + code)

    module Foreground =
      let black = baseToAnsi (int BaseColor.Black) Foreground false
      let red = baseToAnsi (int BaseColor.Red) Foreground false
      let green = baseToAnsi (int BaseColor.Green) Foreground false
      let yellow = baseToAnsi (int BaseColor.Yellow) Foreground false
      let blue = baseToAnsi (int BaseColor.Blue) Foreground false
      let magenta = baseToAnsi (int BaseColor.Magenta) Foreground false
      let cyan = baseToAnsi (int BaseColor.Cyan) Foreground false
      let white = baseToAnsi (int BaseColor.White) Foreground false
      let brightBlack = baseToAnsi (int BaseColor.Black) Foreground true
      let brighRed = baseToAnsi (int BaseColor.Red) Foreground true
      let brighGreen = baseToAnsi (int BaseColor.Green) Foreground true
      let brighYellow = baseToAnsi (int BaseColor.Yellow) Foreground true
      let brighBlue = baseToAnsi (int BaseColor.Blue) Foreground true
      let brighMagenta = baseToAnsi (int BaseColor.Magenta) Foreground true
      let brighCyan = baseToAnsi (int BaseColor.Cyan) Foreground true
      let brighWhite = baseToAnsi (int BaseColor.White) Foreground true

    module Background =
      let black = baseToAnsi (int BaseColor.Black) Background false
      let red = baseToAnsi (int BaseColor.Red) Background false
      let green = baseToAnsi (int BaseColor.Green) Background false
      let yellow = baseToAnsi (int BaseColor.Yellow) Background false
      let blue = baseToAnsi (int BaseColor.Blue) Background false
      let magenta = baseToAnsi (int BaseColor.Magenta) Background false
      let cyan = baseToAnsi (int BaseColor.Cyan) Background false
      let white = baseToAnsi (int BaseColor.White) Background false
      let brightBlack = baseToAnsi (int BaseColor.Black) Background true
      let brighRed = baseToAnsi (int BaseColor.Red) Background true
      let brighGreen = baseToAnsi (int BaseColor.Green) Background true
      let brighYellow = baseToAnsi (int BaseColor.Yellow) Background true
      let brighBlue = baseToAnsi (int BaseColor.Blue) Background true
      let brighMagenta = baseToAnsi (int BaseColor.Magenta) Background true
      let brighCyan = baseToAnsi (int BaseColor.Cyan) Background true
      let brighWhite = baseToAnsi (int BaseColor.White) Background true

  let resetColor = "\u001b[0m"

  let rgbToAnsi (r, g, b) colorType =
    let code =
      match colorType with
      | Foreground -> 38
      | Background -> 48
    sprintf "\u001b[%d;2;%d;%d;%dm" code r g b

  let private hexToRgb (hex: string) =
    try
      let hex =
        if hex.Length = 7 && hex.StartsWith "#" then hex.TrimStart '#'
        else if hex.Length = 6 then hex
        else raise (System.Exception ())

      let i = int32 ("0x" + hex)
      let r = i >>> 16 &&& 0xFF
      let g = i >>> 8 &&& 0xFF
      let b = i &&& 0xFF

      r, g, b
    with
    | _ -> 0, 0, 0

  let hexToAnsi hex =
    hex |> hexToRgb |> rgbToAnsi


module Syntax =
  let private keywords = 
    [|
      "namespace"; "module"; "open"
      "let"; "use"; "mutable"; "inline"; "rec"; "and"
      "type"; "val"; "member"; "interface"; "abstract"; "override"; "default"
      "if"; "then"; "else"; "elif"; "match"; "with"; "for"; "while"; "do"; "done"
      "fun"; "function"; "return"; "yield"
      "seq"; "list"; "array"
      "class"; "inherit"; "constructor"; "new"
      "private"; "public"; "internal"; "protected"
      "async"; "task"; "await"
      "as"; "of"; "in"; "not"; "or"; "and"
      "try"; "with"; "finally"; "raise"; "failwith"
      "unit"; "tuple"; "list"; "array"
      "lazy"
      "extern"; "do"; "mutable"; "match!"; "return!"
    |]

  let private operators = [| "|"; "|>"; "<|"; "<-"; "->"; "+"; "-"; "*"; "/"; "**"; "="; ">"; "<"; ">="; "<="; "<>"; "||"; "&&"; "not"; "~~~"; "|||"; "&&&"; "^^^"; "<<<"; ">>>" |]

  module private Pattern =
    let comment = @"(///.*|//.*)"
    let string = @"""[^""]*"""
    let number = """(?<!""[^""]*)\b(?:\d+\.\d+|\d+|\d+\.?\d*|\b0[xX][0-9a-fA-F]+\b)\b(?![^""]*")"""
    let keyword =
      System.String.Join ("|", keywords) |> sprintf @"\b(%s)\b"
    let operator =
      operators
      |> Array.map (fun x -> x |> System.Text.RegularExpressions.Regex.Escape)
      |> fun x -> System.String.Join ("|", x)
      |> sprintf @"(?<=\s)(%s)(?=\s)"

  let highlight (input: string) =
    let insertAnsi (pattern: string) (color: string) (input: string) =
      System.Text.RegularExpressions.Regex.Replace (input, pattern, fun m -> sprintf "%s%s%s" color m.Value Color.resetColor)

    input
    |> insertAnsi Pattern.number Color.BasicColor.Foreground.green
    |> insertAnsi Pattern.string Color.BasicColor.Foreground.blue
    |> insertAnsi Pattern.keyword Color.BasicColor.Foreground.red
    |> insertAnsi Pattern.operator Color.BasicColor.Foreground.yellow
    |> insertAnsi Pattern.comment Color.BasicColor.Foreground.cyan


module Line =
  let getCharactersWidth (s: string) =
    let calc (c: char) =
      let i = string c |> System.Text.Encoding.UTF8.GetByteCount
      if i = 1 then 1 else 2

    s |> Seq.sumBy (fun c -> c |> calc)

  // https://en.wikipedia.org/wiki/Variation_Selectors_(Unicode_block)
  let containsEmojiModifier (s: string) =
    s |> Seq.exists (fun c -> c = '\uFE0F')

  // 'A' = 0, 'あ' = 1, '🌈' = 2
  let charExpansion (s: string) =
    let byte = s |> System.Text.Encoding.UTF8.GetByteCount
    if byte = 1 then 0
    else if s |> containsEmojiModifier then 2
    else 1

  let strToStrInfo (s: string) =
    s |> System.Globalization.StringInfo

  // [| 'A', 'あ', '☔️' |]
  let getCharacters (s: string) =
    let si = s |> strToStrInfo
    Array.init si.LengthInTextElements (fun i -> si.SubstringByTextElements (i, 1))

  let private repeatSpaces tabStop =
    String.replicate tabStop " "

  let private tabToSpaces editor (s: string) =
    let spaces = editor.TabStop |> repeatSpaces
    s.Replace ("\t", spaces)

  let append (raw: string) editor =
    let chars =
      raw
      |> getCharacters
      |> Array.map (tabToSpaces editor)
    { Raw = raw; Chars = chars }

  let convertCaretXToRenderX (char: string []) caretX =
    let rec loop x i =
      if  i >= caretX then x
      else
        let w = char[i] |> charExpansion
        loop (x + w) (i + 1)

    loop caretX 0

  let convertRenderXToCaretX (char: string []) renderX =
    let rec loop x i =
      if i >= renderX then x
      else
        let w = char[i] |> charExpansion
        loop (x + w) (i + 1)

    loop renderX 0

  let insertChar line index c editor =
    editor |> append (line.Raw.Insert (index, c))

  let deleteChar line index editor =
    if index < 0 || index >= line.Chars.Length then line
    else editor |> append (line.Raw.Remove (index, 1))


module Editor =
  let insertNewLine editor =
    let insertLine index raw editor =
      let line = editor |> Line.append raw
      let lines = editor.Lines |> Array.insertAt index line
      { editor with CaretX = 0; CaretY = editor.CaretY + 1; Lines = lines; IsDirty = true }

    if editor.CaretX = 0 then
      editor |> insertLine editor.CaretY ""
    else
      let splitLineAtCaret editor =
        let raw = editor.Lines[editor.CaretY].Raw
        let fstLine = raw.Substring (0, editor.CaretX)
        let sndLine = raw.Substring editor.CaretX
        fstLine, sndLine

      let fstLine, sndLine = editor |> splitLineAtCaret
      let lines = editor.Lines |> Array.updateAt editor.CaretY (editor |> Line.append fstLine)
      { editor with Lines = lines }
      |> insertLine (editor.CaretY + 1) sndLine

  let insertChar c editor =
    let lines = editor.Lines |> Array.updateAt editor.CaretY (editor |> Line.insertChar editor.Lines[editor.CaretY] editor.CaretX c)
    { editor with CaretX = editor.CaretX + 1; Lines = lines; IsDirty = true }

  let deleteLine lines index =
    lines
    |> Array.mapi (fun i line -> i <> index, line)
    |> Array.filter fst
    |> Array.map snd

  // backspace
  let deletePrevChar editor =
    if editor.CaretX = 0 && editor.CaretY = 0 then editor
    // middle of the line
    else if editor.CaretX > 0 then
      let lines = editor.Lines |> Array.updateAt editor.CaretY (editor |> Line.deleteChar editor.Lines[editor.CaretY] (editor.CaretX - 1))
      { editor with CaretX = editor.CaretX - 1; Lines = lines; IsDirty = true }
    // beginning of the line
    else
      let caretX = editor.Lines[editor.CaretY - 1].Chars.Length
      let lines = editor.Lines |> Array.updateAt (editor.CaretY - 1) (editor |> Line.append (editor.Lines[editor.CaretY - 1].Raw + editor.Lines[editor.CaretY].Raw))
      { editor with CaretX = caretX; CaretY = editor.CaretY - 1; Lines = deleteLine lines editor.CaretY; IsDirty = true }

  // delete
  let deleteChar editor =
    if editor.CaretX = 0 && editor.CaretY = 0 && editor.Lines.Length = 0 then editor
    // middle of the line
    else if editor.CaretX < editor.Lines[editor.CaretY].Chars.Length then
      let lines = editor.Lines |> Array.updateAt editor.CaretY (editor |> Line.deleteChar editor.Lines[editor.CaretY] editor.CaretX)
      { editor with Lines = lines; IsDirty = true }
    // end of the line
    else if editor.CaretY < editor.Lines.Length - 1 then
      let lines = editor.Lines |> Array.updateAt editor.CaretY (editor |> Line.append (editor.Lines[editor.CaretY].Raw + editor.Lines[editor.CaretY + 1].Raw))
      { editor with Lines = deleteLine lines (editor.CaretY + 1); IsDirty = true }
    else
      // last line
      editor


module IO =
  let openFile (filename: string) editor =
    if not (filename |> System.IO.File.Exists) then
      let message = sprintf "'%s' was not found." filename
      editor |> setStatusMessage message
    else
      let lines = filename |> System.IO.File.ReadAllLines |> Array.map (fun x -> editor |> Line.append x)
      { editor with Lines = lines; Filename = Some filename }

  let saveFile editor =
    match editor.Filename with
    | Some filename ->
      let text = editor.Lines |> Array.map (fun x -> x.Raw)
      try
        System.IO.File.WriteAllLines (filename, text, System.Text.Encoding.UTF8)

        let line = editor.Lines.Length
        let size = filename |> System.IO.FileInfo |> fun fi -> fi.Length
        let message = sprintf "'%s' %d L, %d B written." filename line size
        { editor with Filename = Some filename; IsDirty = false }
        |> setStatusMessage message
      with
      | ex -> editor |> setStatusMessage ex.Message
    | None -> editor |> setStatusMessage "No file name."


module Find =
  let find (query: string) editor =
    editor.Lines
    |> Array.mapi (fun i line ->
      let rec loop start acc =
        let pos = line.Raw.IndexOf(query, start, System.StringComparison.OrdinalIgnoreCase)
        if pos = -1 then acc
        else loop (pos + query.Length) (Array.append acc [| (i, pos, pos + query.Length - 1) |])
      loop 0 [||]
    )
    |> Array.concat

  let nextResult matches currentIndex =
    matches
    |> Array.tryItem currentIndex
    |> Option.defaultValue matches[0]

  let previousResult (matches: (int * int * int) []) currentIndex =
    let previousIndex = if currentIndex = 0 then matches.Length - 1 else currentIndex - 1
    matches
    |> Array.tryItem previousIndex
    |> Option.defaultValue matches[0]


module Output =
  let visibleCursor editor (buffer: System.Text.StringBuilder) =
    if editor.CursorShape = CursorShape.None then buffer
    else "\u001b[?25h" |> buffer.Append

  let invisibleCursor (buffer: System.Text.StringBuilder) =
    "\u001b[?25l" |> buffer.Append

  let changeCursorShape editor (buffer: System.Text.StringBuilder) =
    if editor.CursorShape = CursorShape.None then buffer
    else sprintf "\u001b[%d q" (int editor.CursorShape) |> buffer.Append

  let moveCursor editor (buffer: System.Text.StringBuilder) =
    sprintf "\u001b[%d;%dH"
      (editor.CaretY - editor.RowOffset + 1) (editor.RenderX - editor.ColumnOffset + 1) // 1-based
    |> buffer.Append

  let resetCursor (buffer: System.Text.StringBuilder) =
    "\u001b[H" |> buffer.Append

  let writeBuffer (buffer: System.Text.StringBuilder) =
    System.Console.Write (buffer.ToString ())
    buffer

  let scroll editor =
    let renderX =
      if editor.Lines.Length = 0 then 0
      else
        let y = max 0 editor.CaretY
        Line.convertCaretXToRenderX editor.Lines[y].Chars editor.CaretX

    let rowOffset =
      if editor.CaretY < editor.RowOffset then editor.CaretY
      else if editor.CaretY >= editor.RowOffset + editor.ScreenRows then editor.CaretY - editor.ScreenRows + 1
      else editor.RowOffset

    let columnOffset =
      if renderX < editor.ColumnOffset then renderX
      else if renderX >= editor.ColumnOffset + editor.ScreenColumns then renderX - editor.ScreenColumns + 1
      else editor.ColumnOffset

    { editor with RenderX = renderX; RowOffset = rowOffset; ColumnOffset = columnOffset }

  let private drawWelcome editor (buffer: System.Text.StringBuilder) =
    let text = "May your thoughts flow smoothly as you shape the text, like magic in the air."
    let len = min text.Length editor.ScreenColumns
    let pad = (editor.ScreenColumns - len) / 2
    if pad > 0 then
      let spaces = String.replicate pad " "
      sprintf "~%s%s\r\n" spaces text |> buffer.Append
    else
      text.Substring (0, len) |> buffer.Append

  let rec drawLines y editor (buffer: System.Text.StringBuilder) =
    if y < editor.ScreenRows then
      buffer
      |> (fun buffer ->
        let lineOffset = y + editor.RowOffset

        if lineOffset >= editor.Lines.Length then
          if editor.Lines.Length = 0 && y = editor.ScreenRows / 3 then buffer |> drawWelcome editor
          else "~".PadRight (editor.ScreenColumns, ' ') |> buffer.Append

        else
          let removeAnsiCodes line =
            let pattern = @"\u001b\[[0-9;]*[a-zA-Z]"
            System.Text.RegularExpressions.Regex.Replace (line, pattern, "")

          let chars =
            let length = min (max 0 (editor.Lines[lineOffset].Chars.Length - editor.ColumnOffset)) editor.ScreenColumns
            let start = min editor.Lines[lineOffset].Chars.Length editor.ColumnOffset
            editor.Lines[lineOffset].Chars
            |> String.concat ""
            |> fun x -> x.Substring (start, length)

          let source = chars |> Syntax.highlight

          let renderLength = chars |> Line.getCharactersWidth
          let render =
            if chars.Length = 0 then
              let spaces = String.replicate editor.ScreenColumns " "
              spaces
            elif renderLength < editor.ScreenColumns then
              let pad = editor.ScreenColumns - renderLength
              let spaces = String.replicate pad " "
              sprintf "%s%s" source spaces
            else
              let rec getSubstring index acc =
                let length = acc |> removeAnsiCodes |> Line.getCharactersWidth
                if index >= source.Length || length >= editor.ScreenColumns then acc
                else getSubstring (index + 1) (acc + string source[index..index])
              getSubstring 0 ""

          sprintf "%s\r\n" render |> buffer.Append
      )
      |> drawLines (y + 1) editor
    else
      buffer

  let drawStatusBar editor (buffer: System.Text.StringBuilder) =
    let filename =
      match editor.Filename with
      | Some x -> x.Substring (0, min 20 x.Length)
      | None -> "[No Name]"

    let status =
      let mode, color =
        match editor.Mode with
        | Nocturne ->
          " Nocturne ", Color.hexToAnsi "#BC94B7" Color.ColorType.Background + Color.hexToAnsi "#201637" Color.ColorType.Foreground
        | Incantation ->
          " Incantation ", Color.hexToAnsi "#EAB9A8" Color.ColorType.Background + Color.hexToAnsi "#201637" Color.ColorType.Foreground
        | Conjuration ->
          " Conjuration ", Color.hexToAnsi "#C87272" Color.ColorType.Background + Color.hexToAnsi "#201637" Color.ColorType.Foreground

      let filename = sprintf " %s " filename
      let currentLine = editor.CaretY + 1
      let totalLine = max 1 editor.Lines.Length
      let lineStatus = sprintf " %d/%d Ln " currentLine totalLine
      let currentColumn = editor.CaretX + 1
      let totalColumn = if editor.Lines.Length = 0 then 1 else editor.Lines[editor.CaretY].Chars.Length + 1
      let columnStatus = sprintf " %d/%d Col " currentColumn totalColumn
      let editStatus = if editor.IsDirty then " (modified) " else ""
      let pad =
        let len = mode.Length + filename.Length + lineStatus.Length + columnStatus.Length + editStatus.Length
        String.replicate (editor.ScreenColumns - len) " "
      sprintf "%s%s%s%s%s%s%s%s" color mode filename editStatus pad lineStatus columnStatus Color.resetColor

    status |> buffer.Append

  let drawMessageBar editor (buffer: System.Text.StringBuilder) =
    let time = 3000 // ms
    let message =
      if editor.Status.Message.IsSome && editor.Status.Timestamp.IsSome && System.DateTime.Now < editor.Status.Timestamp.Value.AddMilliseconds time then editor.Status.Message.Value
      else ""

    message.PadRight (editor.ScreenColumns, ' ') |> buffer.Append

  let refreshScreen editor =
    let buffer = System.Text.StringBuilder ()
    buffer
    |> invisibleCursor
    |> resetCursor
    |> drawLines 0 editor
    |> drawStatusBar editor
    |> drawMessageBar editor
    |> moveCursor editor
    |> changeCursorShape editor
    |> visibleCursor editor
    |> writeBuffer
    |> ignore
    editor

module Input =
  let moveFirstLine editor =
    let caretX = editor.Lines[0].Chars.Length
    { editor with CaretX = caretX; CaretY = 0 }

  let moveLastLine editor =
    let caretY = editor.Lines.Length - 1
    let caretX = editor.Lines[caretY].Chars.Length
    { editor with CaretX = caretX; CaretY = caretY }

  let moveLeft editor =
    let caretX = max 0 (editor.CaretX - 1)
    { editor with CaretX = caretX }

  let moveDown editor =
    let caretY = max 0 (min (editor.Lines.Length - 1) (editor.CaretY + 1))
    let caretX =
      if caretY = 0 || caretY = editor.Lines.Length - 1 then 0
      else editor.Lines[editor.CaretY + 1].Chars.Length
    { editor with CaretX = caretX; CaretY = caretY }

  let moveUp editor =
    let caretY = if editor.CaretY = 0 then 0 else max 0 (editor.CaretY - 1)
    let caretX =
      if caretY = 0 then 0
      else editor.Lines[editor.CaretY - 1].Chars.Length
    { editor with CaretX = caretX; CaretY = caretY }

  let moveRight editor =
    let rowlen =
      if editor.CaretY >= editor.Lines.Length then 0
      else editor.Lines[editor.CaretY].Chars.Length
    let caretX = min rowlen (editor.CaretX + 1)
    { editor with CaretX = caretX }

  let movePageUp editor =
    let caretY = max 0 (editor.CaretY - editor.ScreenRows)
    let rowlen = if caretY >= editor.Lines.Length then 0 else editor.Lines[caretY].Chars.Length
    let caretX = rowlen
    { editor with CaretX = caretX; CaretY = caretY }

  let movePageDown editor =
    let caretY = min (editor.CaretY + editor.ScreenRows) (editor.Lines.Length -  1)
    let rowlen = if caretY >= editor.Lines.Length then 0 else editor.Lines[caretY].Chars.Length
    let caretX = rowlen
    { editor with CaretX = caretX; CaretY = caretY }


  let processFind (keyBuffer: System.Text.StringBuilder) editor =
    let query = keyBuffer.ToString().Substring 1 // remove '/'
    let matches = editor |> Find.find query
    Debug.writeLog Debug.Debug (sprintf "Query: %s, ResultLength: %d" query matches.Length)

    if matches.Length = 0 then
      let message = sprintf "'%s' was not found." query
      { editor with CursorShape = CursorShape.BlinkingHalfBlock; Mode = Nocturne }
      |> setStatusMessage message
    else
      let rec loop i editor =
        let index, startIndex, _ = Find.nextResult matches i
        let caretX = Line.convertRenderXToCaretX editor.Lines[index].Chars startIndex
        Debug.writeLog Debug.Debug (sprintf "careteX: %d, matches: %d, i: %d, index: %d, startIndex: %d" caretX matches.Length i index startIndex)
        let editor = { editor with CaretX = caretX; CaretY = index; CursorShape = CursorShape.BlinkingHalfBlock } |> Output.scroll |> Output.refreshScreen
        let input = System.Console.ReadKey true
        match input.Key with
        | System.ConsoleKey.Escape -> { editor with CursorShape = CursorShape.BlinkingHalfBlock; Mode = Nocturne }
        | System.ConsoleKey.Enter ->
          let nextIndex = (i + 1) % matches.Length
          loop nextIndex editor
        | _ -> loop i editor

      loop 0 editor

  let handleFind editor =
    let rec loop (keyBuffer: System.Text.StringBuilder) =
      editor |> setStatusMessage (keyBuffer.ToString ()) |> Output.refreshScreen |> ignore
      let input = System.Console.ReadKey true
      match input.Key with
      | System.ConsoleKey.Escape -> { editor with CursorShape = CursorShape.BlinkingHalfBlock; Mode = Nocturne }
      | System.ConsoleKey.Enter ->
        if keyBuffer.Length <= 1 then { editor with CursorShape = CursorShape.BlinkingHalfBlock; Mode = Nocturne }
        else editor |> processFind keyBuffer
      | System.ConsoleKey.Backspace ->
        if keyBuffer.Length <= 1 then keyBuffer |> loop
        else keyBuffer.Remove(keyBuffer.Length - 1, 1) |> loop
      | _ ->
        if input.KeyChar |> System.Char.IsControl then keyBuffer |> loop
        else input.KeyChar |> keyBuffer.Append |> loop

    let keyBuffer = System.Text.StringBuilder ()
    "/" |> keyBuffer.Append |> loop


  let processCommand (keyBuffer: System.Text.StringBuilder) editor =
    match keyBuffer.ToString().Substring 1 with
    | "w" ->
      { editor with CursorShape = CursorShape.BlinkingHalfBlock; Mode = Nocturne } |> IO.saveFile
    | x when x.StartsWith "w " ->
      let filename = keyBuffer.ToString().Substring 3 // remove ":w "
      { editor with CursorShape = CursorShape.BlinkingHalfBlock; Filename = Some filename; Mode = Nocturne } |> IO.saveFile
    | "q" ->
      if editor.IsDirty then
        let message = "WARNING!!! File has unsaved changes."
        { editor with CursorShape = CursorShape.BlinkingHalfBlock; Mode = Nocturne } |> setStatusMessage message
      else
        { editor with CursorShape = CursorShape.BlinkingHalfBlock; IsQuitting = true }
    | "q!" -> { editor with CursorShape = CursorShape.BlinkingHalfBlock; IsQuitting = true }
    | _ ->
      let message = sprintf "Not an editor command: %s" (keyBuffer.ToString())
      { editor with CursorShape = CursorShape.BlinkingHalfBlock; Mode = Nocturne } |> setStatusMessage message

  let handleCommand editor =
    let rec loop (keyBuffer: System.Text.StringBuilder) =
      // refresh only
      editor |> setStatusMessage (keyBuffer.ToString()) |> Output.refreshScreen |> ignore
      let input = System.Console.ReadKey true
      match input.Key with
      | System.ConsoleKey.Escape -> { editor with CursorShape = CursorShape.BlinkingHalfBlock; Mode = Nocturne }
      | System.ConsoleKey.Enter -> editor |> processCommand keyBuffer
      | System.ConsoleKey.Backspace ->
        if keyBuffer.Length <= 1 then keyBuffer |> loop // ':'
        else keyBuffer.Remove(keyBuffer.Length - 1, 1) |> loop // delete last char
      | _ ->
        if input.KeyChar |> System.Char.IsControl then keyBuffer |> loop
        else input.KeyChar |> keyBuffer.Append |> loop

    let keyBuffer = System.Text.StringBuilder ()
    ":" |> keyBuffer.Append |> loop


  let rec processKeyPress (keys: (System.ConsoleKey * System.ConsoleModifiers) list) editor =
    let editor = editor |> Output.scroll |> Output.refreshScreen
    let input = System.Console.ReadKey true
    // Esc to Nocturne
    if input.Key = System.ConsoleKey.Escape then
      { editor with CursorShape = CursorShape.BlinkingHalfBlock; Mode = Nocturne } |> processKeyPress []

    else
      let c = input.KeyChar.ToString ()
      let keys = keys @ [ (input.Key, input.Modifiers) ]

      match editor.Mode, keys with
      // move cursor
      | Nocturne, [ System.ConsoleKey.H, _ ] | Nocturne, [ System.ConsoleKey.LeftArrow, _ ] -> editor |> moveLeft  |> processKeyPress []
      | Nocturne, [ System.ConsoleKey.J, _ ] | Nocturne, [ System.ConsoleKey.DownArrow, _ ] -> editor |> moveDown  |> processKeyPress []
      | Nocturne, [ System.ConsoleKey.K, _ ] | Nocturne, [ System.ConsoleKey.UpArrow, _ ] -> editor |> moveUp  |> processKeyPress []
      | Nocturne, [ System.ConsoleKey.L, _ ] | Nocturne, [ System.ConsoleKey.RightArrow, _ ] -> editor |> moveRight  |> processKeyPress []
      // move page
      | Nocturne, [ System.ConsoleKey.G, System.ConsoleModifiers.None ] -> editor |> processKeyPress keys // add 'g'
      | Nocturne, [ System.ConsoleKey.G, System.ConsoleModifiers.None; System.ConsoleKey.G, System.ConsoleModifiers.None ] -> editor |> moveFirstLine |> processKeyPress []
      | Nocturne, [ System.ConsoleKey.G, System.ConsoleModifiers.Shift ] -> editor |> moveLastLine |> processKeyPress []
      | Nocturne, [ System.ConsoleKey.PageUp, _ ] -> editor |> movePageUp  |> processKeyPress []
      | Nocturne, [ System.ConsoleKey.PageDown, _ ] -> editor |> movePageDown |> processKeyPress []

      // '/': switch to conjuration mode (search)
      | Nocturne, [ System.ConsoleKey.Oem2, _ ] ->
        { editor with CursorShape = CursorShape.None; Mode = Conjuration }
        |> handleFind
        |> processKeyPress []

      // ':': switch to conjuration mode
      | Nocturne, [ System.ConsoleKey.Oem1, _ ] ->
        { editor with CursorShape = CursorShape.None; Mode = Conjuration }
        |> handleCommand 
        |> fun editor -> if editor.IsQuitting then editor else editor |> processKeyPress []

      // 'i': switch to incantation mode
      | Nocturne, [ System.ConsoleKey.I, _ ] ->
        let caretX = max 0 (editor.CaretX - 1)
        let lines = if editor.Lines.Length = 0 then [| { Raw = ""; Chars = [||] } |] else editor.Lines
        { editor with CaretX = caretX; Lines = lines; CursorShape = CursorShape.BlinkingBar; Mode = Incantation } |> processKeyPress []

      // 'a': switch to incantation mode
      | Nocturne, [ System.ConsoleKey.A, _ ] ->
        let rowlen = if editor.CaretY >= editor.Lines.Length then 0 else editor.Lines[editor.CaretY].Chars.Length
        let caretX = min rowlen (editor.CaretX + 1)
        let lines = if editor.Lines.Length = 0 then [| { Raw = ""; Chars = [||] } |] else editor.Lines
        { editor with CaretX = caretX; Lines = lines; CursorShape = CursorShape.BlinkingBar; Mode = Incantation } |> processKeyPress []

      // backspace: delete a character
      | Incantation, [ System.ConsoleKey.Backspace, _ ] ->
        editor |> Editor.deletePrevChar |> processKeyPress []

      // delete: delete a character
      | Incantation, [ System.ConsoleKey.Delete, _ ] ->
        editor |> Editor.deleteChar  |> processKeyPress []

      // enter: insert a new line
      | Incantation, [ System.ConsoleKey.Enter, _ ] ->
        editor |> Editor.insertNewLine  |> processKeyPress []

      // move cursor
      | Incantation, [ System.ConsoleKey.LeftArrow, _ ] -> editor |> moveLeft |> processKeyPress []
      | Incantation, [ System.ConsoleKey.DownArrow, _ ] -> editor |> moveDown |> processKeyPress []
      | Incantation, [ System.ConsoleKey.UpArrow, _ ] -> editor |> moveUp |> processKeyPress []
      | Incantation, [ System.ConsoleKey.RightArrow, _ ] -> editor |> moveRight  |> processKeyPress []

      // insert char
      | Incantation, _ ->
        editor |> Editor.insertChar c  |> processKeyPress []

      | _, _ ->
        editor |> processKeyPress []

let initEditor title =
  System.Console.Title <- title

  {
    CaretX = 0; CaretY = 0; RenderX = 0
    RowOffset = 0; ColumnOffset = 0
    ScreenRows = System.Console.WindowHeight - 2 // status bar + message bar
    ScreenColumns = System.Console.WindowWidth
    Lines = [||]
    TabStop = 4
    IsDirty = false
    Filename = None
    Status = { Message = None; Timestamp = None }
    CursorShape = CursorShape.BlinkingHalfBlock
    Mode = Nocturne
    IsQuitting = false
  }

let main args =
  System.Console.InputEncoding <- System.Text.Encoding.UTF8
  System.Console.OutputEncoding <-System.Text.Encoding.UTF8

  let altBuffer = System.Text.StringBuilder () |> Terminal.enableAlternativeBuffer |> Output.writeBuffer

  "filo editor"
  |> initEditor
  |> setStatusMessage "Howdy?"
  |> (match args with
      | [| filename |] -> IO.openFile filename
      | _ -> id
    )
  |> Input.processKeyPress [] // main loop
  |> ignore

  altBuffer |> Terminal.disableAlternativeBuffer |> Output.writeBuffer |> ignore
  0

let args = fsi.CommandLineArgs |> Array.skip 1
main args

