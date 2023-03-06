use rand::prelude::SliceRandom;
use rand::Rng;
use clap::Parser;

mod player;

use crate::player::RandomPlayer;

type Score = i32;
type Seat = usize;

#[derive(Debug,Copy,Clone,PartialEq,Eq,Hash,Ord,PartialOrd)]
enum Color {
    Red,
    Blue,
    Yellow,
    Green,
    Wild,
}

#[derive(Debug,Copy,Clone,Eq,PartialEq)]
enum Kind {
    Number {num:u8}, 
    Draw2,
    Draw4,
    Reverse,
    Skip,
    Wild,
}

#[derive(Copy,Clone,Eq,PartialEq)]
struct Card {
    kind: Kind,
    color: Color,
}

impl Card {
    fn to_score(&self) -> Score {
	match self.kind {
	    Kind::Number {num} => num as Score,
	    Kind::Reverse | Kind::Skip | Kind::Draw2 => 20 as Score,
	    Kind::Wild | Kind::Draw4 => 50 as Score,
	}
    }

    fn is_wild(&self) -> bool {
	self.kind == Kind::Wild
    }

    fn is_draw2(&self) -> bool {
	self.kind == Kind::Draw2
    }

    fn is_draw4(&self) -> bool {
	self.kind == Kind::Draw4
    }

    fn is_same_as(&self, other: &Card) -> bool {
	if (self.is_wild() || self.is_draw4()) && Card::is_same_kind(*self, *other) {
	    return true;
	}
	else if Card::is_same_color(*self, *other) && Card::is_same_kind(*self, *other) {
	    return true;
	}
	else {
	    return false;
	}
    }	    

    fn is_same_color(card1: Card, card2: Card) -> bool {
	return card1.color == card2.color;
    }

    fn is_same_kind(card1: Card, card2: Card) -> bool {
	match card1.kind {
	    Kind::Number{num:num1} => {
		match card2.kind {
		    Kind::Number{num:num2} => {
			if num1 == num2 {
			    return true;
			}
		    },
		    _ => {},
		}
	    },
	    _ => {
		match card2.kind {
		    Kind::Number{num:_} => {},
		    _ => {
			if card1.kind == card2.kind {
			    return true;
			}
		    },
		}
	    },
	}
	return false;
    }
}

fn col2chr(c: Color) -> char {
    match c {
	Color::Red    => 'r',
	Color::Blue   => 'b',
	Color::Yellow => 'y',
	Color::Green  => 'g',
	Color::Wild   => 'w',
    }
}

impl std::fmt::Display for Card {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
	match *self {
	    Card {kind: Kind::Number {num}, color: c} => write!(f,"{}{}",num,col2chr(c)),
	    Card {kind: Kind::Draw2, color: c} => write!(f,"d{}",col2chr(c)),
	    Card {kind: Kind::Draw4, color: c} => write!(f,"D{}",col2chr(c)),
	    Card {kind: Kind::Reverse, color: c} => write!(f,"r{}",col2chr(c)),
	    Card {kind: Kind::Skip, color: c} => write!(f,"s{}",col2chr(c)),
	    Card {kind: Kind::Wild, color: c} => write!(f,"w{}",col2chr(c)),
	}
    }
}

impl std::fmt::Debug for Card {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
	std::fmt::Display::fmt(self,f)
    }
}

fn generate_card_pile(rng: &mut rand::rngs::StdRng) -> Vec<Card> {
    let qty1 = 1;
    let qty2 = 2;
    let qty4 = 5;

    let mut cards = vec![];
    for k in [Kind::Reverse, Kind::Skip, Kind::Draw2] {
	for cl in [Color::Red, Color::Blue, Color::Yellow, Color::Green] {
	    for _ in 0..qty2 {
		let c = Card {kind: k, color: cl};
		cards.push(c);
	    }
	}
    }
    for cl in [Color::Red, Color::Blue, Color::Yellow, Color::Green] {
	for _ in 0..qty1 {
	    let c = Card {kind: Kind::Number{num:0}, color: cl};
	    cards.push(c);
	}
    }
    for cl in [Color::Red, Color::Blue, Color::Yellow, Color::Green] {
	for n in 1..10 {
	    for _ in 0..qty2 {
		let c = Card {kind: Kind::Number{num:n}, color: cl};
		cards.push(c);
	    }
	}
    }
    for k in [Kind::Wild, Kind::Draw4] {
	for _ in 0..qty4 {
	    let c = Card {kind: k, color: Color::Wild};
	    cards.push(c);
	}
    }
    cards.shuffle(rng);
    cards
}

struct Engine {
    seed: u64,
    n_games: usize,
    n_players: usize,
    n_initial_cards: usize,
    logging: bool,
}

impl Engine {
    fn new(seed: u64, n_games: usize, n_players: usize, n_initial_cards: usize, logging: bool) -> Self {
	Engine {
	    seed: seed,
	    n_games: n_games,
	    n_players: n_players,
	    n_initial_cards: n_initial_cards,
	    logging: logging,
	}
    }

    fn run(&mut self) {
	for n in 0..self.n_games {
	    let mut g = Game::new(self.seed + n as u64, self.n_players, self.n_initial_cards);
	    let result = g.run();
	    if self.logging {
		g.print_result();
	    }
	    println!("game[{}] seed: {} dealer: {} result: {:?}", &n, &g.stage.seed, &g.stage.dealer, &result);
	}
    }
}

#[derive(Debug,Clone)]
struct GameResult {
    scores: Vec<Score>,
    dealer: Seat,
    total_turns: usize,
    n_reshuffles: usize,
    winner: Option<Seat>,
    state: GameState,
}

impl GameResult {
    fn new(scores: Vec<Score>, dealer: Seat, total_turns: usize, n_reshuffles: usize, state: GameState) -> Self {
	GameResult {
	    scores: scores,
	    dealer: dealer,
	    total_turns: total_turns,
	    n_reshuffles: n_reshuffles,
	    winner: None,
	    state: state,
	}
    }
}

struct Config {
    n_initial_cards: usize,
}

struct Game {
    #[allow(dead_code)]
    config: Config,
    n_players: usize,
    stage: Stage,
    players: Vec<Box<dyn Player>>,
}

fn random_choose(rng: &mut rand::rngs::StdRng, modulo: usize) -> usize {
    return rng.gen::<usize>() % modulo;
}

impl Game {
    fn new(seed: u64, n_players: usize, n_initial_cards: usize) -> Self {
	let config = Config {n_initial_cards: n_initial_cards};
	let mut rng: rand::rngs::StdRng = rand::SeedableRng::seed_from_u64(seed);
	let draw_pile = generate_card_pile(&mut rng);
	let dealer = random_choose(&mut rng, n_players);
	let mut stage = Stage {
	    seed: seed,
	    rng: rng,
	    dealer: dealer,
	    turn: dealer,
	    prev_turn: dealer,
	    total_turns: 0,
	    is_reversed: false,
	    is_skip: false,
	    is_discarded: false,
	    need_challenge_check: false,
	    playerinfo: vec![],
	    draw_pile: draw_pile,
	    discard_pile: vec![],
	    n_reshuffles: 0,
	    draw1_in_process: None,
	    num_draw2: 0,
	    num_draw4: 0,
	    game_state: None,
	    record: vec![],
	};
	for seat in 0..n_players {
	    stage.playerinfo.push(PlayerInfo::new(seat, vec![]));
	}
	let mut players = vec![];
	for seat in 0..n_players {
	    let hand = stage.draw_cards_from_pile(config.n_initial_cards).unwrap();
	    stage.playerinfo[seat].draw_cards(hand.clone());
	    players.push(Box::new(RandomPlayer::init(seat, n_players, hand.clone())) as Box<dyn Player>);
	}
	while stage.draw_pile.last().unwrap().is_draw4() {
	    stage.draw_pile.shuffle(&mut stage.rng);
	}

	let first_card = stage.draw_pile.pop().unwrap();
	assert!(!first_card.is_draw4());
	stage.discard_pile = vec![first_card];
	stage.event(&Action::Nop, &Event::GameStart);
	let g = Game {
	    config: config,
	    n_players: n_players,
	    stage: stage,
	    players: players,
	};
	g
    }

    fn run(&mut self) -> GameResult {
	// It is like the dealer draws the first card from the draw pile and discards it.
	self.process_discard();
	self.next_turn();

	loop {
	    self.process_turn();
	    if let Some(_) = &self.stage.game_state {
		break;
	    }
		
	    if !self.stage.draw1_in_process.is_none() {
		self.process_draw1();
	    }
	    if let Some(_) = &self.stage.game_state {
		break;
	    }

	    if self.stage.need_challenge_check {
		self.process_draw4();
	    }
	    if let Some(_) = &self.stage.game_state {
		break;
	    }

	    if self.stage.is_discarded {
		self.process_discard();
	    }
	    self.next_turn();
	}

	let mut result = GameResult::new(vec![0; self.n_players],
					 self.stage.dealer,
					 self.stage.total_turns,
					 self.stage.n_reshuffles,
					 self.stage.game_state.unwrap());
	let mut total_minus: Score = 0;
	let mut winner = None;
	for p in 0..self.n_players {
	    let score = Game::calc_score(&self.stage.playerinfo[p].hand);
	    if score == 0 && self.stage.playerinfo[p].hand.is_empty() {
		winner = Some(p);
	    }
	    total_minus += score;
	}
	if let Some(winner) = winner {
	    result.winner = Some(winner);
	    result.scores[winner] = total_minus;
	}
	else {
	    result.winner = None;
	}
	self.stage.event(&Action::Nop, &Event::GameOver{result:result.clone()});
	result
    }

    fn process_turn(&mut self) {
	let turn = self.stage.turn;
	let actions = self.stage.find_playable_actions(turn);
	let ctx = self.stage.to_ctx(turn);
	let act = self.players[turn].select_action(ctx, actions);
	self.stage.event(&act, &Event::Nop);

	match act {
	    Action::Discard {card} => {
		self.stage.playerinfo[turn].discard(card);
		self.stage.discard_to_pile(card);
		self.stage.is_discarded = true;
		self.stage.draw1_in_process = None;
		if card.is_draw4() {
		    self.stage.need_challenge_check = true;
		}
		else {
		    self.stage.need_challenge_check = false;
		}
		if self.stage.playerinfo[turn].hand.is_empty() {
		    let winner = turn;
		    if card.is_draw2() || card.is_draw4() {
			self.next_turn();
			let next_turn = self.stage.turn;
			let card_num = if card.is_draw2() {2} else {4};
			if let Some(cards) = self.stage.draw_cards_from_pile(card_num) {
			    self.stage.playerinfo[next_turn].draw_cards(cards);
			}
		    }
		    self.stage.game_state = Some(GameState::Win {winner: winner});
		}
	    },
	    Action::Draw1 => {
		self.stage.is_discarded = false;
		self.stage.need_challenge_check = false;
		let card = match self.stage.draw_cards_from_pile(1) {
		    Some(cards) => cards[0],
		    None => {
			self.stage.game_state = Some(GameState::EmptyDrawPile);
			return;
		    },
		};
		self.stage.draw1_in_process = Some(card);
	    },
	    Action::Draw2 => {
		self.stage.is_discarded = false;
		self.stage.need_challenge_check = false;
		self.stage.draw1_in_process = None;
		self.stage.num_draw2 = 0;
		let cards = match self.stage.draw_cards_from_pile(2) {
		    Some(cards) => cards,
		    None => {
			self.stage.game_state = Some(GameState::EmptyDrawPile);
			return;
		    },
		};
		self.stage.playerinfo[turn].draw_cards(cards);
	    },
	    _ => {
		self.stage.is_discarded = false;
		self.stage.draw1_in_process = None;
		self.stage.need_challenge_check = false;
	    },
	}
    }

    fn process_draw1(&mut self) {
	let turn = self.stage.turn;
	let card = self.stage.draw1_in_process.unwrap();
	let mut actions = vec![Action::KeepIt {card:card}];
	if self.stage.is_playable(card) {
	    actions.push(Action::DiscardIt {card});
	}
	let ctx = self.stage.to_ctx(turn);
	let act = self.players[turn].select_action(ctx, actions);
	self.stage.event(&act, &Event::Nop);

	match act {
	    Action::KeepIt {card:card2} => {
		assert!(card2 == card);
		self.stage.playerinfo[turn].draw_cards(vec![card]);
		self.stage.is_discarded = false;
		self.stage.draw1_in_process = None;
		self.stage.need_challenge_check = false;
	    },
	    Action::DiscardIt {card: card2} => {
		self.stage.discard_to_pile(card2);
		self.stage.is_discarded = true;
		self.stage.draw1_in_process = None;
		if card2.is_draw4() {
		    self.stage.need_challenge_check = true;
		}
		else {
		    self.stage.need_challenge_check = false;
		}
	    },
	    _ => panic!("Error: act = {:?}", &act),
	}
    }

    fn process_draw4(&mut self) {
	let prev_turn = self.stage.turn;
	self.next_turn();
	let turn = self.stage.turn;
	assert!(self.stage.last_card().is_draw4());
	let mut actions = vec![Action::Draw4];
	if !self.stage.playerinfo[prev_turn].hand.is_empty() {
	    actions.push(Action::Challenge {against: prev_turn, by: turn});
	}
	let ctx = self.stage.to_ctx(turn);
	let act = self.players[turn].select_action(ctx, actions);
	self.stage.event(&act, &Event::Nop);

	match act {
	    Action::Challenge {against:_, by:_} => {
		let mut is_guilty = false;
		let prev_card = self.stage.discard_pile[self.stage.discard_pile.len()-2];
		for c in &self.stage.playerinfo[prev_turn].hand {
		    if c.is_wild() {
			is_guilty = true;
			break;
		    }
		    if Card::is_same_color(prev_card, *c) || Card::is_same_kind(prev_card, *c) {
			is_guilty = true;
			break;
		    }
		}
		if is_guilty {
		    // prev_turn draw4
		    self.stage.event(&Action::Nop, &Event::GuiltyDraw4{player:prev_turn});
		    let cards = match self.stage.draw_cards_from_pile(4) {
			Some(cards) => cards,
			None => {
			    self.stage.game_state = Some(GameState::EmptyDrawPile);
			    return;
			},
		    };
		    self.stage.playerinfo[prev_turn].draw_cards(cards);
		    self.stage.num_draw4 = 0;
		    self.stage.is_discarded = false;
		    self.stage.draw1_in_process = None;
		    self.stage.need_challenge_check = false;
		}
		else {
		    // turn draw6
		    self.stage.event(&Action::Nop, &Event::NotGuiltyDraw6{player:turn});
		    let cards = match self.stage.draw_cards_from_pile(20) {
			Some(cards) => cards,
			None => {
			    self.stage.game_state = Some(GameState::EmptyDrawPile);
			    return;
			},
		    };
		    self.stage.playerinfo[turn].draw_cards(cards);
		    self.stage.num_draw4 = 0;
		    self.stage.is_discarded = false;
		    self.stage.draw1_in_process = None;
		    self.stage.need_challenge_check = false;
		}
	    },
	    Action::Draw4 => {
		self.stage.event(&Action::Nop, &Event::Draw4{player:turn});
		let cards = match self.stage.draw_cards_from_pile(4) {
		    Some(cards) => cards,
		    None => {
			self.stage.game_state = Some(GameState::EmptyDrawPile);
			return;
		    },
		};
		self.stage.playerinfo[turn].draw_cards(cards);
		self.stage.num_draw4 = 0;
		self.stage.is_discarded = false;
		self.stage.draw1_in_process = None;
		self.stage.need_challenge_check = false;
	    },
	    _ => panic!(""),
	}
    }

    fn process_discard(&mut self) {
	match self.stage.last_card() {
	    Card {kind: Kind::Reverse, color: _} => {
		self.stage.is_reversed ^= true;
	    },
	    Card {kind: Kind::Skip, color: _} => {
		self.stage.is_skip = true;
	    },
	    Card {kind: Kind::Draw2, color: _} => {
		self.stage.num_draw2 = 1;
	    },
	    Card {kind: Kind::Draw4, color: _} => {
		self.stage.num_draw4 = 1;
	    },
	    _ => {
		// doing nothing for Number's or Wild's
	    },
	}
	self.stage.is_discarded = false;
    }

    fn next_turn(&mut self) {
	if self.n_players != 2 {
	    self.stage.total_turns += 1;
	    self.stage.prev_turn = self.stage.turn;
	    let steps = if self.stage.is_skip {2} else {1};
	    if !self.stage.is_reversed {
		self.stage.turn = (self.stage.turn + steps) % self.n_players;
	    }
	    else {
		self.stage.turn = (self.n_players + self.stage.turn - steps) % self.n_players;
	    }
	    self.stage.is_skip = false;
	}
	else {
	    self.stage.total_turns += 1;
	    self.stage.prev_turn = self.stage.turn;
	    if !self.stage.is_reversed && !self.stage.is_skip {
		self.stage.turn ^= 1;
	    }
	    self.stage.is_skip = false;
	    self.stage.is_reversed = false;
	}	    
    }

    fn calc_score(cards: &Vec<Card>) -> Score {
	let score: Score = cards.iter().map(|c| c.to_score()).sum();
	return score;
    }

    fn print_result(&self) {
	for r in self.stage.record.iter() {
	    println!("turn:{} total:{} lastcard:{:?} action:{:?} event:{:?} players:{:?}",
		     &r.turn,
		     &r.total_turns,
		     &r.discard_pile_top,
		     r.action,
		     r.event,
		     r.hands);
	}
    }
}

#[allow(dead_code)]
struct Context {
    seat: Seat, // to whom this is sent
    is_reversed: bool,
    hand: Vec<Card>,
    discard_pile: Vec<Card>,
    n_cards: Vec<usize>, // the number of cards of each player
    draw1_in_process: Option<Card>,
    num_draw2: usize,
    num_draw4: usize,
}

#[derive(Debug,Eq,PartialEq,Copy,Clone)]
enum GameState {
    Win {winner: Seat},
    EmptyDrawPile,
    TooManyReshuffles,
}

#[derive(Debug,Clone)]
enum Event {
    GameStart,
    GameOver {result: GameResult},
    GuiltyDraw4 {player: Seat},
    Draw4 {player: Seat},
    NotGuiltyDraw6 {player: Seat},
    Nop,
}

struct Record {
    turn: Seat,
    total_turns: usize,
    discard_pile_top: Card,
    action: Action,
    event: Event,
    hands: Vec<Vec<Card>>,
}

struct Stage {
    seed: u64,
    rng: rand::rngs::StdRng,
    dealer: Seat,
    turn: Seat,
    prev_turn: Seat,
    total_turns: usize,
    is_reversed: bool,
    is_skip: bool,
    is_discarded: bool,
    need_challenge_check: bool,
    playerinfo: Vec<PlayerInfo>,
    draw_pile: Vec<Card>,
    discard_pile: Vec<Card>,
    n_reshuffles: usize,
    draw1_in_process: Option<Card>,
    num_draw2: usize,
    num_draw4: usize,
    game_state: Option<GameState>,
    record: Vec<Record>,
}

impl Stage {
    fn event(&mut self, a: &Action, e: &Event) {
	self.record.push(Record {
	    turn: self.turn,
	    total_turns: self.total_turns,
	    discard_pile_top: self.discard_pile.last().unwrap().clone(),
	    action: a.clone(),
	    event: e.clone(),
	    hands: self.playerinfo.clone().iter().map(|pi| pi.hand.clone()).collect::<Vec<Vec<Card>>>()});
	return;
    }
    
    fn to_ctx(&self, seat: Seat) -> Context {
	let mut n_cards = vec![];
	for p in &self.playerinfo {
	    n_cards.push(p.hand.len());
	}
	Context {
	    seat: seat,
	    is_reversed: self.is_reversed,
	    hand: self.playerinfo[seat].hand.clone(),
	    discard_pile: self.discard_pile.clone(),
	    n_cards: n_cards,
	    draw1_in_process: self.draw1_in_process.clone(),
	    num_draw2: self.num_draw2,
	    num_draw4: self.num_draw4,
	}
    }

    fn is_playable(&self, card: Card) -> bool {
	let last_card = self.last_card();
	if card.is_wild() || card.is_draw4() {
	    return true;
	}
	else {
	    if Card::is_same_color(card, last_card) || Card::is_same_kind(card, last_card) {
		return true;
	    }
	    else {
		return false;
	    }
	}
    }
    
    fn find_playable_actions(&self, seat: Seat) -> Vec<Action> {
	let mut actions;
	if self.num_draw2 > 0 {
	    actions = vec![Action::Draw2];
	}
	else if self.num_draw4 > 0 {
	    actions = vec![Action::Draw4,
			   Action::Challenge {against: self.prev_turn, by: self.turn}];
	}
	else {
	    let hand = &self.playerinfo[seat].hand;
	    actions = vec![Action::Draw1];
	    for c in hand.iter().filter(|&c| self.is_playable(*c)) {
		let act = Action::Discard {card: *c};
		actions.push(act);
	    }
	}
	return actions;
    }

    fn draw_cards_from_pile(&mut self, num: usize) -> Option<Vec<Card>> {
	if self.draw_pile.len() < num {
	    let last_played = self.discard_pile.pop().unwrap();
	    self.discard_pile.shuffle(&mut self.rng);
	    self.discard_pile.extend(&self.draw_pile);
	    self.draw_pile = self.discard_pile.clone();
	    self.discard_pile = vec![last_played];
	    self.n_reshuffles += 1;
	}
	if self.draw_pile.len() < num {
	    self.game_state = Some(GameState::EmptyDrawPile);
	    return None;
	}
	if self.n_reshuffles > 1000 {
	    self.game_state = Some(GameState::TooManyReshuffles);
	    return None;
	}
	
	let mut cards = vec![];
	for _ in 0..num {
	    let mut card = self.draw_pile.pop().unwrap();
	    if card.is_wild() || card.is_draw4() {
		card.color = Color::Wild;
	    }
	    cards.push(card);
	}
	return Some(cards);
    }

    fn discard_to_pile(&mut self, card: Card) {
	self.discard_pile.push(card);
    }

    fn last_card(&self) -> Card {
	*self.discard_pile.last().unwrap()
    }
}

trait Player {
    fn init(seat: Seat, n_players: Seat, initial_hand: Vec<Card>) -> Self where Self: Sized;
    fn select_action(&mut self, ctx: Context, actions: Vec<Action>) -> Action;
}

#[derive(Debug,Clone)]
struct PlayerInfo {
    seat: Seat,
    hand: Vec<Card>,
}

impl PlayerInfo {
    fn new(seat: usize, hand: Vec<Card>) -> Self {
	PlayerInfo {
	    seat: seat,
	    hand: hand,
	}
    }

    fn discard(&mut self, card: Card) {
	let pos = self.hand.iter().position(|&c| c.is_same_as(&card)).unwrap();
	self.hand.remove(pos);
    }

    fn draw_cards(&mut self, cards: Vec<Card>) {
	self.hand.extend(&cards);
    }
}

#[derive(Debug,Clone,Eq,PartialEq)]
enum Action {
    Discard {card: Card},
    Draw1, // can be single
    Draw2, // single
    Draw4, // with Challenge or can be single at the last turn
    // Draw6, // single
    KeepIt {card: Card},
    DiscardIt {card: Card},
    Challenge {against: Seat, by: Seat},
    Nop,
}

impl Action {
    fn is_challenge(&self) -> bool {
	match *self {
	    Action::Challenge {against:_,by:_} => true,
	    _ => false,
	}
    }
}

fn unixtime_now() -> u64 {
    std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap()
        .as_secs()
}

#[derive(Parser,Debug)]
struct Args {
    #[clap(short,long)]
    game_num: usize,
    
    #[clap(short,long)]
    player_num: usize,
    
    #[clap(short,long)]
    initial_card_num: usize,
    
    #[clap(short,long)]
    verbose: bool,

    #[clap(short,long)]
    seed: u64,
}

fn main() {
    let args = Args::parse();
    
    let mut seed = args.seed;
    if seed == 0 {
	seed = unixtime_now();
    }

    // n_player:4 initial_card_num:7 seed:1650721487 => NotGuiltyDraw6
    
    let n_games = args.game_num;
    let n_players = args.player_num;
    let n_initial_cards = args.initial_card_num;
    let logging = if args.verbose {true} else {false};
    let mut e = Engine::new(seed, n_games, n_players, n_initial_cards, logging);
    e.run();
}
