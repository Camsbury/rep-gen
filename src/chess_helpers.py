import chess
import chess.engine as ngn
import json
import os

STOCKFISH_PATH = os.getenv("STOCKFISH_PATH")

def ucis_to_fen(ucis):
    ucis = ucis.split(",")
    board = chess.Board()
    for uci in ucis:
        board.push_uci(uci)
    return board.fen()

def sans_to_ucis(sans):
    sans = sans.split(",")
    board = chess.Board()
    for san in sans:
        board.push_san(san)
    return ",".join(move.uci() for move in board.move_stack)

# TODO: do the work of getting the uci/score stuff before
# sending back to haskell as a json string
def ucis_to_engine_candidates(ucis, color, depth, move_count):
    ucis = ucis.split(",")
    board = chess.Board()
    for uci in ucis:
        board.push_uci(uci)
    engine = ngn.SimpleEngine.popen_uci(STOCKFISH_PATH)
    engine.configure({"Hash": 2048, "Threads": 7})
    info = engine.analyse(
        # chess.Board("1k6/8/1K6/8/8/8/8/3Q4 w - - 0 1"), - to see how mates look
        board,
        ngn.Limit(depth=depth),
        multipv=move_count,
    )
    engine.quit()

    if color == "white":
        color_score = lambda x: x.white()
    else:
        color_score = lambda x: x.black()

    ret = json.dumps([{"uci": x["pv"][0].uci(), "score": str(color_score(x["score"]))}
           for x in info])
    return ret
