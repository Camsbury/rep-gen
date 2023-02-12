import chess

def ucis_to_fen(ucis):
    ucis = ucis.split(",")
    board = chess.Board()
    for uci in ucis:
        board.push_uci(uci)
    return board.fen()
