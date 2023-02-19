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

def fen_to_engine_candidates(fen, depth, move_count):
    board = chess.Board(fen)
    engine = ngn.SimpleEngine.popen_uci(STOCKFISH_PATH)
    engine.configure({"Hash": 2048, "Threads": 7})
    info = engine.analyse(
        # chess.Board("4q3/8/8/8/8/6k1/8/6K1 b - - 0 1"), # to see how mates look
        board,
        ngn.Limit(depth=depth),
        multipv=move_count,
    )
    engine.quit()
    ret = json.dumps([{"uci": x["pv"][0].uci(), "score": str(x["score"].white())}
           for x in info])
    return ret
