import chess
import chess.engine as ngn
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
        board,
        ngn.Limit(depth=depth),
        multipv=move_count,
    )
    engine.quit()
    print(info)
    return info
