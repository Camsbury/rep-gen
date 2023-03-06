import chess
import chess.engine as ngn
import chess.pgn as pgn
import json
import os

STOCKFISH_PATH = os.getenv("STOCKFISH_PATH")

def ucis_to_fen(ucis):
    if len(ucis) == 0:
        ucis = []
    else:
        ucis = ucis.split(",")
    board = chess.Board()
    for uci in ucis:
        board.push_uci(uci)
    return board.fen()

def sans_to_ucis(sans):
    if len(sans) == 0:
        sans = []
    else:
        sans = sans.split(",")
    board = chess.Board()
    for san in sans:
        board.push_san(san)
    return ",".join(move.uci() for move in board.move_stack)

def fen_to_engine_candidates(fen, depth, move_count):
    board = chess.Board(fen)
    engine = ngn.SimpleEngine.popen_uci(STOCKFISH_PATH)
    engine.configure({"Hash": 2048, "Threads": 7})
    info = engine.analyse(
        board,
        ngn.Limit(depth=depth),
        multipv=move_count,
    )
    engine.quit()
    try:
        ret = json.dumps([{"uci": x["pv"][0].uci(), "score": str(x["score"].white())}
               for x in info])
        return ret
    except KeyError:
        print(f"Bad input for fen_to_engine_candidates: {fen}")
        return json.dumps([])

def tree_to_pgn(input_tree):
    tree = json.loads(input_tree)
    game = pgn.Game()
    stack = [(game, x) for x in reversed(list(filter(lambda y: not y[1]['removed'], tree['nodeResponses'])))]

    while stack:
        (node, entry) = stack.pop()
        uci = entry[0]
        node = node.add_variation(chess.Move.from_uci(uci))
        responses = [(node, z) for z in reversed(list(filter(lambda y: not y[1]['removed'], entry[1]['nodeResponses'])))]
        stack += responses

    return str(game)

