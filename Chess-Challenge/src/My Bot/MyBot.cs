using ChessChallenge.API;
using System;

public class MyBot : IChessBot
{
#if UCI
    public ulong nodes = 0;
#endif

    readonly int[] weights = new int[3216];
    readonly (ulong, Move, int, int, byte)[] tt = new (ulong, Move, int, int, byte)[1048576];

    public MyBot()
    {
        // Extract weights
        // Weights are quantised into 6 bits, so 16 values are
        // packed into one `decimal`, allowing all weights to
        // be packed into 194 decimals
        for (int i = 0; i < 3216;)
        {
            var packed = decimal.GetBits(packedWeights[i / 16]);
            int num = i % 16 * 6;
            uint adj = (uint)packed[num / 32] >> num % 32 & 63;
            if (num == 30) adj += ((uint)packed[1] & 15) << 2;
            if (num == 60) adj += ((uint)packed[2] & 3) << 4;
            weights[i++] = (int)adj - 31;
        }
    }

#if UCI
    public Move Think(Board board, Timer timer)
    {
        return ThinkInternal(board, timer);
    }

    public Move ThinkInternal(Board board, Timer timer, int maxDepth = 50, bool report = true)
#else
    public Move Think(Board board, Timer timer)
#endif
    {
        Move bestMoveRoot = default;
        var killers = new Move[128];
        var history = new int[4096];
        int iterDepth = 1;
#if UCI
        nodes = 0;
        for (iterDepth = 1; iterDepth <= maxDepth && timer.MillisecondsElapsedThisTurn < timer.MillisecondsRemaining / 30;)
#else
        while (timer.MillisecondsElapsedThisTurn < timer.MillisecondsRemaining / 30)
#endif
        {
#if UCI
            int score =
#endif
            Search(-30000, 30000, iterDepth++, 0);
#if UCI
            if (report && timer.MillisecondsElapsedThisTurn < timer.MillisecondsRemaining / 30)
            {
                ulong time = (ulong)timer.MillisecondsElapsedThisTurn;
                ulong nps = nodes * 1000 / Math.Max(time, 1);
                Console.WriteLine(
                    $"info depth {iterDepth} score cp {score} time {time} nodes {nodes} nps {nps}"
                );
            }
#endif
        }

        return bestMoveRoot;

        int Search(int alpha, int beta, int depth, int ply)
        {
            bool inCheck = board.IsInCheck();

            // Check extensions
            if (inCheck)
                depth++;

            bool qs = depth <= 0;
#if UCI
            nodes++;
#endif
            ulong key = board.ZobristKey;
            var (ttKey, ttMove, ttDepth, score, ttFlag) = tt[key % 1048576];
            int bestScore = -30000;

            // Check for draw by repetition
            if (ply > 0
                && board.IsRepeatedPosition())
                return 0;

            // Stand Pat
            if (qs
                && (bestScore = alpha = Math.Max(alpha, Evaluate())) >= beta)
                return alpha;

            // TT Cutoffs
            if (beta - alpha == 1
                && ttKey == key
                && ttDepth >= depth
                && (score >= beta ? ttFlag > 0 : ttFlag < 2))
                return score;

            // Reverse Futility Pruning
            if (!qs
                && !inCheck
                && depth <= 8
                && Evaluate() >= beta + 120 * depth)
                return beta;

            // Generate moves
            var moves = board.GetLegalMoves(qs);

            // Checkmate/Stalemate
            if (moves.Length == 0)
                return qs ? alpha : inCheck ? ply - 30_000 : 0;

            // Score moves
            int moveIdx = 0;
            var scores = new int[moves.Length];
            foreach (Move move in moves)
                scores[moveIdx++] = -(
                    move == ttMove
                        ? 900_000_000
                        : move.IsCapture
                            ? 100_000_000 * (int)move.CapturePieceType - (int)move.MovePieceType
                            : move == killers[ply]
                                ? 80_000_000
                                : history[move.RawValue & 4095]
                );

            Array.Sort(scores, moves);

            ttMove = default;
            moveIdx = ttFlag = 0;

            foreach (Move move in moves)
            {
                if (timer.MillisecondsElapsedThisTurn >= timer.MillisecondsRemaining / 15)
                    return 30000;

                board.MakeMove(move);

                // Principal Variation Search + Late Move Reductions
                if (moveIdx++ == 0
                    || qs
                    || depth < 2
                    || move.IsCapture
                    || (score = -Search(-alpha - 1, -alpha, depth - 2 - moveIdx / 16, ply + 1)) > alpha)
                    score = -Search(-beta, -alpha, depth - 1, ply + 1);

                board.UndoMove(move);

                if (score > bestScore)
                {
                    bestScore = score;
                    ttMove = move;
                    if (score > alpha)
                    {
                        alpha = score;
                        ttFlag = 1;

                        if (ply == 0)
                            bestMoveRoot = move;

                        if (alpha >= beta)
                        {
                            // Quiet cutoffs update tables
                            if (!move.IsCapture)
                            {
                                killers[ply] = move;
                                history[move.RawValue & 4095] += depth;
                            }

                            ttFlag++;

                            break;
                        }
                    }
                }
            }

            tt[key % 1048576] = (key, ttMove, depth, bestScore, ttFlag);

            return bestScore;
        }

        int Evaluate()
        {
            var accumulators = new int[2, 8];
            int mat = 0;

            // Adds a feature (colour, piece, square) to given accumulator
            void updateAccumulator(int side, int feature)
            {
                for (int i = 0; i < 8;)
                    accumulators[side, i] += weights[feature * 8 + i++];
            }

            // Initialise with input biases
            updateAccumulator(0, 384);
            updateAccumulator(1, 384);

            for (int stm = 2; --stm >= 0;)
            {
                for (var p = 0; p <= 5; p++)
                    for (ulong mask = board.GetPieceBitboard((PieceType)p + 1, stm > 0); mask != 0;)
                    {
                        mat += (int)(0x3847D12C4B064 >> 10 * p & 0x3FF);
                        int sq = BitboardHelper.ClearAndGetIndexOfLSB(ref mask);

                        // Input is horizontally mirrored
                        sq = sq / 8 * 4 + (0x1BE4 >> 2 * (sq & 7) & 3);

                        // Add feature from each perspective
                        updateAccumulator(0, 192 - stm * 192 + p * 32 + sq);
                        updateAccumulator(1, stm * 192 + p * 32 + (sq ^ 28));
                    }
                mat = -mat;
            }

            // Initialise with output bias
            int bucket = (BitboardHelper.GetNumberOfSetBits(board.AllPiecesBitboard) - 2) / 4,
                eval = 8 * weights[3208 + bucket],
                widx = 3080 + 16 * bucket;

            // Compute hidden -> output layer
            for (int i = 0; i < 16;)
                eval += Math.Clamp(accumulators[i / 8 ^ (board.IsWhiteToMove ? 0 : 1), i % 8], 0, 32) * weights[widx + i++];

            // Scale + Material Factoriser
            return eval * 400 / 1024 + (board.IsWhiteToMove ? mat : -mat);
        }
    }

    readonly decimal[] packedWeights = {
        38985286316542769292061308895m,38985286316542769292061308895m,53781948643130774187838675105m,56297108775864259554795452452m,51305459234015596280068581474m,51364696155337672294312581221m,57534458891697179924230449379m,51364710325878996916351674405m,
        53762605613250232607915518308m,55078535093485584617035937830m,54960944180071751568094792105m,50049403534027508862425643305m,58631861893580628673154140458m,54924089649088519276362328175m,38985286316542769292061308895m,38985286316542769292061308895m,
        37666051116413573387580762079m,26505550326490917506442479906m,32714602856875703077161826593m,37608036915725840842835777699m,31437981764966573229001562275m,33894523824360893055430707492m,36389439615803078923753781412m,37608051007939175362103757092m,
        40084233467809156059250726885m,38807310217132327802653534437m,35054795110083189692655642788m,35055715896625742602649110692m,41360844607663891050992158821m,33894528477516083099297822821m,31455525210154650445383129313m,31456416486463783572553271908m,
        17840899993349153182057932774m,19077626529735140615631304675m,22714684580376435052621506532m,22752765669943093425637320868m,22714080117502995323179927524m,25228957423603122072225470567m,25228348240614392284448987111m,25228659764634650665594808549m,
        28923137367947870998500906983m,27626861786808761096035797220m,25209307660108396640264165543m,22694142140925619279923677351m,31378130275431419761257781285m,22751858758810852575773108389m,25266415166431967836907403494m,22771508523512564966519392291m,
        51339644754151011590951332394m,46369141965918390363886893551m,46407525141690116830096710252m,51321511233432046910812633712m,42693709668972991811127261744m,48864973966840485999505746609m,46407832238527790997249903216m,50083571265664105726751709808m,
        47626736419691668431552047728m,50083278407496504700623829679m,48845338297901213050506771182m,52520779817119479875696519918m,50063940248762813526590561006m,50045206620888968101536926509m,48824786487468326258689116844m,51282542197264545458804685550m,
        48570355653994784069837908917m,51008759248889031776488897591m,46075737296683115657160809529m,47313989230076570209085275063m,51027502175056230175841963064m,48551929268227043517491022906m,47333029591600395462731615290m,49829173486539627109416406202m,
        48570672120644442241518263293m,48589717571091159214809398588m,38667161183036782057556012219m,47351479879755082922358979900m,41142399462504912927681407866m,49846995410605694859065421049m,41200441626206554724420545596m,47371124775560068862391868603m,
        24091920033220504349000202206m,15483456713927982581515442141m,36509401515659560137819023391m,39042705499180882490745030751m,38985588548015687418731169759m,42757134872642105891551574048m,45194938660086682346668034015m,47670218996997515943099103263m,
        47631840545933867281944803359m,50126770358036460602854864991m,52564257889980144858488502623m,52544631295577943602729977888m,53840562283647727564808890649m,48869189405171878457740228831m,50127960310389280518419098267m,41460589821410697075384752478m,
        38985286316542769292061308895m,38985286316542769292061308895m,649648864644015898830730655m,691634603064501188149871903m,9336088038823817410902890207m,709485586588851007261427361m,24171118915292232890221197087m,24228835383335951051943184291m,
        21655646438883924035990054624m,24190452132678392107339732962m,24169905189878136235829094048m,24190754364095300969940240290m,24150562377899209207502464607m,24152370970457166592586929951m,38985286316542769292061308895m,38985286316542769292061308895m,
        55176688810908299534003516894m,51599160615813195025199068378m,42837173671719529001015437149m,49103937946606695641663797145m,54093788782181298003353134746m,50361523104266375944687445720m,46607817875582828775891646234m,49123280832318564294794274457m,
        50341585269480343836802258650m,44133448954286305037568554648m,51542057974728002622040401689m,47847273863702523608141235992m,50245178224656630958426777432m,46570964670548532219633559320m,50226458762711315560744712024m,47789557170877098186287441688m,
        37920457672817876040278968213m,37920769128834598018159322643m,41712574628166956026408838868m,35484179363013483203492157141m,35561253176798695854724200214m,36761112055036029594805585749m,37979709427611150972657436566m,39256939558357301220059485971m,
        35503531837645845013581887254m,39256330368469333678200817428m,39217961067045065845230630675m,39217649537242203273223300947m,45407968141199967093556336532m,42893398009165505009604966292m,35482677494935064238486026130m,35465753197508079098548270743m,
        29298719778946035678383813844m,28080117758909131530909897877m,30497978841718442978828207316m,29356743349518319801803064533m,28041441571668117417369105556m,25584597353542440108737270934m,28022108126023212339016582293m,26803506252443099926978192533m,
        26745487330450313449196573843m,29260355192916401351576081554m,31735933041203466687617787027m,32954530340008513601852503188m,31716887884734273008026076243m,32973868355852277339199194388m,32954223536067784689878910036m,36706426897717176124691010771m,
        34600090317108595106646528014m,32163505335964628376602165256m,33303843281115295657886589000m,33305349862443236192260575237m,35798759361977357596832669704m,36998920394441019762172350533m,36959630154253384486044913672m,36998925117942128056763514886m,
        32007260883317273559887327303m,35741637387550323526068420677m,35721388099145419787529162888m,34464704987289279135536722053m,30750280478023010910602592457m,36920637935105343374739177735m,29531093030944966067855101956m,38139546836294364033902239753m,
        62428667338106430444396239263m,63609230556440168905836726239m,58736093159521818005230467039m,56239670711810685154170398687m,53900092152902611820644563363m,50146986813525863391549360096m,46432859739524729385298835297m,43938250901867628431629768353m,
        37709265264864154237674596063m,38908822056933275647385155104m,32756900569944950571183507423m,37651237043443099088403957471m,27882213859977319264082078047m,33956764534944314896524114015m,31672796043967394251847113372m,38927569920144240557390952735m,
        77951834227685229421589604694m,77255492988390748189496643008m,60794759050910424584603915843m,45842764292555829618585894912m,37140311336129463099419791360m,32189452779617686758038442053m,28475920505799517287394582987m,24762379007402337605383357200m,
        64135519411417978034518230291m,
    };
}