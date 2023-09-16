using ChessChallenge.API;
using System;

public class MyBot : IChessBot
{
#if UCI
    public ulong nodes = 0;
#endif

    readonly int[] weights = new int[3097];
    readonly (ulong, Move, int, int, byte)[] tt = new (ulong, Move, int, int, byte)[1048576];

    public MyBot()
    {
        // Extract weights
        // Weights are quantised into 6 bits, so 16 values are
        // packed into one `decimal`, allowing all weights to
        // be packed into 194 decimals
        for (int i = 0; i < 3097;)
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
        var history = new int[2, 64, 64];
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
            ulong key = board.ZobristKey, moveIdx = 0;
            var (ttKey, ttMove, ttDepth, score, ttFlag) = tt[key % 1048576];

            // Check for draw by repetition
            if (ply > 0 && board.IsRepeatedPosition())
                return 0;

            // Stand Pat
            if (qs && (alpha = Math.Max(alpha, Evaluate())) >= beta)
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
            var scores = new int[moves.Length];
            foreach (Move move in moves)
                scores[moveIdx++] = -(
                    move == ttMove
                        ? 900_000_000
                        : move.IsCapture
                            ? 100_000_000 * (int)move.CapturePieceType - (int)move.MovePieceType
                            : move == killers[ply]
                                ? 80_000_000
                                : history[ply % 2, move.StartSquare.Index, move.TargetSquare.Index]
                );

            Array.Sort(scores, moves);

            ttMove = default;
            moveIdx = ttFlag = 0;

            foreach (Move move in moves)
            {
                if (timer.MillisecondsElapsedThisTurn >= timer.MillisecondsRemaining / 15)
                    return 30000;

                board.MakeMove(move);

                // Principle Variation Search + Late Move Reductions
                if (moveIdx++ == 0
                    || qs
                    || depth < 2
                    || move.IsCapture
                    || (score = -Search(-alpha - 1, -alpha, depth - 2, ply + 1)) > alpha)
                    score = -Search(-beta, -alpha, depth - 1, ply + 1);

                board.UndoMove(move);

                if (score > alpha)
                {
                    alpha = score;
                    ttMove = move;
                    ttFlag = 1;

                    if (ply == 0)
                        bestMoveRoot = move;

                    if (alpha >= beta)
                    {
                        // Quiet cutoffs update tables
                        if (!move.IsCapture)
                        {
                            killers[ply] = move;
                            history[ply % 2, move.StartSquare.Index, move.TargetSquare.Index] += depth;
                        }

                        ttFlag++;

                        break;
                    }
                }
            }

            tt[key % 1048576] = (key, ttMove, depth, alpha, ttFlag);

            return alpha;
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
            int eval = weights[3096];

            // Compute hidden -> output layer
            for (int i = 0; i < 16;)
                eval += Math.Clamp(accumulators[i / 8 ^ (board.IsWhiteToMove ? 0 : 1), i % 8], 0, 32) * weights[3080 + i++];

            // Scale + Material Factoriser
            return eval * 400 / 1024 + (board.IsWhiteToMove ? mat : -mat);
        }
    }

    readonly decimal[] packedWeights = {
        38985286316542769292061308895m,38985286316542769292061308895m,39122513637992042301919201503m,39121592632430709110277540003m,36646628910805741982231427232m,37864309928704598277485893730m,35427738974041105959342708831m,36645708128912025112453458019m,
        36646331331444269717219313695m,37845255183442420018842839206m,31752292658689360299282004068m,35369648778533782908236081323m,24324945135858758007197648676m,39198609263726257455860697323m,38985286316542769292061308895m,38985286316542769292061308895m,
        46550460966867184037492258078m,46569506124507837064088491552m,46569798912310709000494224670m,46550762834017685230361093667m,46531122586737586456827368994m,46550465251124832371081586276m,52682746045912291927509026277m,44055242215307593832045594151m,
        46512091299950416597167760936m,47749724092500563396232345064m,51444792130028792366335387110m,50226506364449075805767185831m,48989151974626916768457282854m,53940019231028947012801377704m,62585935498132039293937247519m,62509773170306804749086602402m,
        38891320436441309404649047845m,38987439704005053796281832933m,42700943349502691755880166950m,40245015271609602319100241445m,40225374800716511098759894503m,38987741716387701602546443816m,38948753786089089422071268903m,41404988746286407425324578279m,
        41463319490887803842135164392m,41443976530182284192352929191m,38910970131859943786078115301m,38910360871589985423177909737m,47595893070018161684748007013m,37691758924240044697550685670m,38909751612543035420661089761m,38930005772095871617610384673m,
        39163644781202910593468171750m,39181764715787341616353215911m,39182685359438815085981963941m,37963176861768740825496655528m,36687764774167040656110880485m,37944145652244459824447854311m,36726459845145684597134436070m,36725246270711115136142251751m,
        36706819451444572330716931815m,37963497765958633369014220456m,36706819524042879730169521769m,36706512496417074391254410794m,39163658947150013708829304363m,36667831591385515967987688939m,39221078200043999548615001514m,39201433154323193815820768747m,
        60290595754700181376134087154m,52861123460999973209861841521m,59051432621306792027258946098m,56594588402063940464171868658m,59051432548690751500245074610m,54118712824517012111026606769m,54061000788341842729139507761m,51642535018974520830442934898m,
        51585422795940143129702244851m,50347468364700124485813608948m,54023524090204443330691277299m,50327832911213339188299899254m,51547332109382286116157206771m,52784355935802127707466699253m,57755789550108850678125117745m,57736725354312780285613773043m,
        37727701157749017098335676577m,37747639210345738544265160668m,32775940928009197533892311200m,32796195310002419153172822108m,34033526085649434279515912348m,35310454058673899844071720987m,37805374716616896957403498653m,39062652921535210423205099486m,
        41538537647515113459493504993m,44033444202902844448412919519m,46547419549652302201039484771m,46489367640179362988294264546m,50240074020913665914369861410m,48925017734459291217912783649m,52677240137886375306656593631m,52598622178086319297507747746m,
        38985286316542769292061308895m,38985286316542769292061308895m,56129981502617047026770269281m,51312709921721643190026152023m,53823958973238851318406600285m,52586608865655399774647650267m,50127965271284132437503657887m,46414135711072922217200117919m,
        47651778310373395722230708193m,43956394241009933738425870623m,42718756498823933881437800482m,43975444336541091365518997727m,45194636651181679521076750306m,43936461420298111582903359582m,38985286316542769292061308895m,38985286316542769292061308895m,
        26352569044085881275209893599m,40009204063017711919389104159m,45020211329759134825862293725m,38791820783602036179268434264m,41287043602652294733557295515m,41306688795947339137340828055m,42563371905462478794178853273m,42543722137373786941098187095m,
        43761724270411137925743008025m,42581812818094974230160628120m,42542536750789110350842837274m,42562172422106020157825636696m,45037754771362628205394679834m,41323934798792325623029062040m,41265301599033787189639120923m,43780471991597187248798706010m,
        33780818387881875873485699413m,33761466203822338097755597082m,37475304990899811480329144536m,33742435068048736518773819799m,36295691047690082763689318678m,37495554646006075228370655636m,40028858849735690698919406040m,38791216318385246688388991446m,
        38771878080064787532651325911m,37532729188763943293968079254m,37572023928769316739096922457m,37591059933348165539957991894m,36314131892228697399867885911m,37590762351607671029156432341m,32542594860185366432665463000m,38809671180198137930598455640m,
        48616444819700179867801651871m,52350216937062792500579580381m,52369564472579415240136108702m,53627144834066496287542524507m,51131322276796757632857848415m,53627154353739078420054325852m,51131029563844058618354117216m,53626559408196573106744769052m,
        51131038934789766587863181791m,53645604787144155728586652189m,48635206856658227289087179103m,52388014832179220382508034462m,51092051000979199844973439327m,51130727184779468378772519262m,51091437092146917867785976094m,49911823004768244167199861021m,
        60780010051989708158515485397m,57162606491874749108458040083m,58247943333116308411708566546m,57105192111191003939009944468m,57145081720644651891050891986m,58343731891001417319811341141m,58401458022794915286760504210m,57144472899710979003655076629m,
        59639397987121815877259574033m,57201599218344558052055133138m,59658136115964346465638615891m,58439539108902492415740287954m,57181958453365271999523535763m,59677172046773969418660031246m,53449404633885450749464484691m,60895471690033803942937688848m,
        38998504219012984572944422045m,35246942875735814202235680542m,47665298216993089568253006751m,41456316524753654759483635616m,46450360825044233666959373791m,45211249639015498309028935713m,42757125576581403771702732448m,45232410637012772042469406817m,
        43975732173717235297778001888m,45233024544690166519589632095m,39004629128503668660110215008m,43975751062066001253555365983m,32776238583446011341233579678m,42699125248961254068997449758m,27843811722014966982236042845m,41499879912872262891285112799m,
        4932105667857367576247878863m,12090229875531648m,
    };
}