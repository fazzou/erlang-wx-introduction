-module(wxExample).
-author("Szymon Tracz").

-include_lib("wx/include/wx.hrl").

-export([start/0]).

start() ->
  State = make_window(),
  loop (State).

make_window() ->
  Server = wx:new(),
  Frame = wxFrame:new(Server, -1, "Obliczanie wyznacznika", [{size,{500, 500}}]),
  redraw(2, null, Frame).

redraw(N, PanelOld, Frame) ->

  case PanelOld of
    null -> ok;
    _ -> wxPanel:destroy(PanelOld)
  end,

  Panel = wxPanel:new(Frame),

  Field = wxStaticText:new(Panel, 2001," Wyznacznik: ",[]),
  CalculateBtn = wxButton:new(Panel, 101, [{label, "&Calculate"}]),
  PlusBtn = wxButton:new(Panel, 102, [{label, "+"}]),
  MinusBtn = wxButton:new(Panel, 103, [{label, "-"}]),
  ExitBtn  = wxButton:new(Panel, ?wxID_EXIT, [{label, "E&xit"}]),

  OuterSizer  = wxBoxSizer:new(?wxHORIZONTAL),
  MainSizer   = wxBoxSizer:new(?wxVERTICAL),
  Mtx = drawBlankMtx(Panel, MainSizer, N),

  ButtonSizer = wxBoxSizer:new(?wxHORIZONTAL),

  wxSizer:addSpacer(MainSizer, 10),

  wxSizer:add(MainSizer, Field, []),

  wxSizer:add(ButtonSizer, CalculateBtn,  []),
  wxSizer:add(ButtonSizer, PlusBtn,  []),
  wxSizer:add(ButtonSizer, MinusBtn,  []),
  wxSizer:add(ButtonSizer, ExitBtn,  []),

  wxSizer:addSpacer(MainSizer, 10),

  wxSizer:add(MainSizer, ButtonSizer, []),

  wxSizer:add(OuterSizer, MainSizer, []),

  wxPanel:setSizer(Panel, OuterSizer),

  wxFrame:show(Frame),

  wxPanel:fit(Panel),
  wxFrame:fit(Frame),

  wxFrame:connect(Frame, close_window),
  wxPanel:connect(Panel, command_button_clicked),

  {Panel, Frame, Mtx, Field}.


loop(State) ->
  {Panel, Frame, Mtx, Field}  = State,
  receive

    #wx{event=#wxClose{}} ->
      wxWindow:destroy(Frame),
      ok;

    #wx{id = ?wxID_EXIT, event=#wxCommand{type = command_button_clicked} } ->
      wxWindow:destroy(Frame),
      ok;

    #wx{id = 101, event=#wxCommand{type = command_button_clicked}} ->

      MtxToCalc = lists:map(
        (fun (Row) -> lists:map
          (fun (Cell) -> list_to_integer(wxTextCtrl:getValue(Cell)) end, Row) end),
        Mtx),
      wxStaticText:setLabel(Field,
        io_lib:format(" Wyznacznik: ~.3f",[calculateDeterminant(MtxToCalc)])),
      loop(State);

    #wx{id = 102, event=#wxCommand{type = command_button_clicked}} ->

      loop(redraw(length(Mtx) + 1, Panel, Frame));

    #wx{id = 103, event=#wxCommand{type = command_button_clicked}} ->

      loop(redraw(length(Mtx) - 1, Panel, Frame));

    Msg ->
      io:format("loop default triggered: Got ~n ~p ~n", [Msg]),
      loop(State)

  end.

drawBlankMtx(_, _, 0) -> [];

drawBlankMtx(Panel, MainSizer, N) ->
  NRowSizer = wxStaticBoxSizer:new(?wxHORIZONTAL, Panel, [{label, ""}]),
  wxSizer:add(MainSizer, NRowSizer),
  [addCellsToRow(Panel, NRowSizer, N, N)] ++ drawBlankMtx(Panel, MainSizer, N - 1, N).

drawBlankMtx(_, _, 0, _) -> [];

drawBlankMtx(Panel, MainSizer, NR, NC) ->
  NRowSizer = wxStaticBoxSizer:new(?wxHORIZONTAL, Panel, [{label, ""}]),
  wxSizer:add(MainSizer, NRowSizer),
  [addCellsToRow(Panel, NRowSizer, NC, NC)] ++ drawBlankMtx(Panel, MainSizer, NR - 1, NC).

addCellsToRow(_, _, _, 0) -> [];

addCellsToRow(Panel, NRowSizer, Row, Col) ->
  NNCell = wxTextCtrl:new(Panel, Row*100+Col, [{value, "0"}]),
  wxSizer:add(NRowSizer, NNCell),
  [NNCell] ++ addCellsToRow(Panel, NRowSizer, Row, Col-1).

calculateDeterminant(Mtx) ->
    calculateDeterminant(Mtx, 1, 1).

calculateDeterminant(Mtx, Row, Col) ->
  Len = length(Mtx) + 1,
  case {Row, Len} of
    {_, 2} -> L = getNth(Mtx, Row),
      getNth(L, Col);
    {Len, _} -> 0;
    {_, _} -> [H | T] = getNth(Mtx, Row),
      math:pow(-1, Row + Col)
      *calculateDeterminant(lists:delete([H | T], Mtx), 1, Col + 1)*getNth([H|T], Col)
      +calculateDeterminant(Mtx, Row + 1, Col)
  end.

getNth([H | T], N) ->
  case N of
    1 -> H;
    _ -> getNth(T, N - 1)
  end.
