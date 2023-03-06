use std::collections::HashMap;
use std::cmp::Ordering;

use crate::Card;
use crate::Color;
use crate::Action;
use crate::Context;
use crate::Seat;
use crate::Player;

#[allow(dead_code)]
pub struct RandomPlayer {
    seat: Seat,
    n_players: usize,
    hand: Vec<Card>,
}

impl Player for RandomPlayer {
    fn init(seat: Seat, n_players: Seat, initial_hand: Vec<Card>) -> Self where Self: Sized {
	RandomPlayer {
	    seat: seat,
	    n_players: n_players,
	    hand: initial_hand,
	}
    }

    fn select_action(&mut self, ctx: Context, actions: Vec<Action>) -> Action {
	for act in actions.iter() {
	    match *act {
		Action::Draw2 => {
		    return act.clone();
		},
		Action::KeepIt {card:_} => {
		    if actions.len() == 1 {
			return act.clone();
		    }
		},
		Action::DiscardIt {card:_} => {
		    let mut card = ctx.draw1_in_process.unwrap();
		    if card.is_wild() || card.is_draw4() {
			let color = majority_color(&ctx.hand);
			card.color = color;
		    }
		    return Action::DiscardIt {card: card};
		},
		Action::Draw1 => {
		    if actions.len() == 1 {
			return act.clone();
		    }
		},
		_ => {},
	    }
	}
	// in case of challenge
	if let Some(pos) = actions.iter().position(|a| a.is_challenge()) {
	    let act = &actions[pos];
	    let who = match act {
		Action::Challenge {against: a, by: _} => *a as Seat,
		_ => panic!(),
	    };
	    if ctx.n_cards[who] > 10 {
		return act.clone();
	    }
	    else {
		let pos2 = actions.iter().position(|a| *a == Action::Draw4).unwrap();
		return actions[pos2].clone();
	    }
	}
	for act in actions.iter() {
	    match *act {
		Action::Discard {card} => {
		    if !card.is_wild() && !card.is_draw4() {
			return Action::Discard {card:card};
		    }
		},
		_ => {},
	    }
	}
	for act in actions.iter() {
	    match *act {
		Action::Discard {mut card} => {
		    if card.is_wild()  || card.is_draw4() {
			let color = majority_color(&ctx.hand);
			card.color = color;
		    }
		    return Action::Discard {card: card};
		},
		_ => {},
	    }
	}
	return actions[0].clone();
    }
}

fn majority_color(hand: &Vec<Card>) -> Color {
    if hand.iter().any(|&c| !c.is_wild()) {
	let mut counter = HashMap::new();
	for c in hand.iter() {
	    if !c.is_wild() {
		let count = counter.entry(c.color).or_insert(0);
		*count += 1;
	    }
	}
	let mut counter: Vec<_> = counter.iter().collect();
	counter.sort_by(|a,b| {
	    match b.1.cmp(a.1) {
		Ordering::Equal => b.0.cmp(a.0),
		other => other,
	    }
	});
	return *counter[0].0;
    }
    else {
	return Color::Red;
    }
}
