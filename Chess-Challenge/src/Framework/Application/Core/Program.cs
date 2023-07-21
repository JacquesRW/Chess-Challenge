using System;
using ChessChallenge.API;

namespace ChessChallenge.Application {
    static class Program {
        public static void Main() {
            String initial_pos = "PLACEHOLDER";
            MyBot bot = new MyBot();
            Chess.Board fen_board = new Chess.Board();
            fen_board.LoadPosition(initial_pos);
            
            Board bot_board = new Board(fen_board);
            Timer timer = new Timer(1000);
            bot.Think(bot_board, timer);
        }
    }
}