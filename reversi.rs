use std::fmt;
use std::collections::HashMap;
use std::cmp::max;
#[macro_use] extern crate text_io;

#[derive(PartialEq, PartialOrd, Clone, Copy)]
enum Disk {
    White,
    Black
}

type Pos = (i32,i32);
type Board = HashMap<Pos,Disk>;
type Value = i32;


impl fmt::Display for Disk {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Disk::White => write!(f, "o"),
            &Disk::Black => write!(f, "x")
        }
    }
}


fn showDisk (d: &Option<&Disk>) -> &'static str {
    match d {
        &Option::None => " ",
        &Option::Some(&Disk::White) => "o",
        &Option::Some(&Disk::Black) => "x"
    }
}

fn showDiskOnBoard (p: &Pos, board: &Board) -> &'static str {
    showDisk(&board.get(&p))
}

fn showBoard (board: &Board) -> String {
    let mut init = " 01234567\n".to_string();
    for y in 0..7{
        init = init + "|";
        for x in 0..7{
            let p = (x,y);
            init = init + showDiskOnBoard (&p,board);
        }
        init = init + "|" + &(y.to_string()) + "\n";
    }
    init
}

fn value(b: &Board, d: &Disk) -> Value {
    b.values().filter(|v| v == &d).count() as i32
}

fn all_<F,P>(cond: F,i: &Vec<P>) -> bool
    where F: Fn(&P) -> bool{
    i.into_iter().fold(true, |sum, v| sum & cond(v))
}


fn between(a: &Pos,b: &Pos) -> Vec<Pos> {
    let dist = |pa: &Pos, pb: &Pos| { max((pa.0-pb.0).abs(), (pa.1-pb.1).abs()) };
    let direct = |pa: &Pos, pb: &Pos| { ((pb.0-pa.0)/dist(pa,pb), (pb.1-pa.1)/dist(pa,pb)) };
    let plus = |i: i32, pa: &Pos, pb: &Pos| { (pa.0+pb.0*i,pa.1+pb.1*i) };
    if a.0 == b.0 || a.1 == b.1 || (a.0 - b.0).abs() == (a.1 - b.1).abs() {
        (1..(dist(a,b) -1)).map(|i| plus(i,a,&direct(a,b))).collect::<Vec<Pos>>()
    } else {
        vec![]
    }
}

fn allsame(board: &Board,disk: &Disk,posa: &Pos, posb: &Pos) -> bool {
    all_ (|pos| board.get(pos) == Option::Some(disk), &between(posa,posb))
}

fn ndisk(disk: &Disk) -> Disk {
    match disk {
        &Disk::White => Disk::Black,
        &Disk::Black => Disk::White
    }
}

fn pos9x9() -> Vec<Pos> {
    let mut ss: Vec<(i32,i32)> = Vec::with_capacity(9);
    for i in -1i32..1i32 {
        for j in -1i32..1i32 {
            ss.push((i,j))
        }
    }
    ss
}

fn mix(a: &Vec<&Pos>, b: &Vec<Pos>) -> Vec<Pos> {
    let mut ss: Vec<(i32,i32)> = Vec::with_capacity(100);
    for &&(x,y) in a.iter() {
        for &(xx,yy) in b.iter() {
            ss.push((x+xx,y+yy))
        }
    }
    ss
}

fn copy(a: Vec<&Pos>) -> Vec<Pos> {
    let mut ss: Vec<Pos> = Vec::new();
    for &&(x,y) in a.iter() {
        ss.push((x,y))
    }
    ss
}

fn next(board: &Board, disk: &Disk) -> Vec<Pos> {
    let r: Disk = ndisk(disk);
    let n: Vec<&Pos> = board.iter().filter(|&(_,v)| v == &r).map(|(k,_)| k).collect();
    let n_: Vec<&Pos> = board.iter().filter(|&(_,v)| v == disk).map(|(k,_)| k).collect();
    let s: Vec<Pos> = copy(mix(&n,&pos9x9()).iter().filter(|&&(x,y)| {
        0<=x&&x<8&&0<=y&&y<8
    }).collect());
    let empty: Vec<Pos> = copy(s.iter().filter(|&p| board.get(&p) == None).collect());
    copy (empty.iter().filter(|&e| {
        n_.iter().any(|n| allsame(board,&r,&e,n))
    }).collect())
}


fn put(board: &Board,disk: &Disk,pos: &Pos) -> Board {
    let r  = ndisk(disk);
    let mut br = board.clone();
    let b = board.clone();
    let bb = board.clone();
    let n_ = b.iter().filter(|&(_,v)| v == disk).map(|(k,_)| k);
    let n__ = n_.map(
        |n| if allsame(&bb,&r,pos,n) {
            between(pos,n)
        } else {
            vec![]
        }
    ).flat_map(|x| x);
    br.insert(*(pos),disk.clone());
    for i in n__ {
        br.insert(i,disk.clone());
    }
    br
}

fn nboard(board: &Board, disk: &Disk) -> Vec<(Value,Pos,Board)> {
    next(board,disk).iter().map(|n| {
        let nb = put(board,disk,n);
        let nb2 = nb.clone();
        (value(&nb2,disk),n.clone(),nb)
    }).collect()
}


fn maxmin(board: &Board,disk: &Disk) -> Option<Pos> {
    let nb = nboard(board,disk);
    match nb.iter().max_by(|&&(a,_,_), &&(b,_,_)| a.partial_cmp(&b).unwrap()) {
        Option::None => Option::None,
        Option::Some(&(_,p,_)) => Option::Some(p.clone())
    }
}

fn main_(disk: &Disk,board: &Board){
    let nd = ndisk(disk);
    let p_ = next(board,disk);
    let p__= next(board,&nd);
    print!("{}",showBoard(board));
    if p_.len() == 0  && p__.len() == 0 {
        if value(board,&Disk::Black) > value(board,&Disk::White) {
            println!("black win")
        } else if value(board,&Disk::Black) < value(board,&Disk::White) {
            println!("white win")
        } else {
            println!("draw")
        }
    }else{
        let b = if p_.len() == 0 {
            board.clone()
        } else {
            let (i, j): (i32,i32);
            scan!("{} {}\n", i, j);
            let p: Pos = (i,j);
            if ! p_.iter().any(|pp| pp == &p) {
                main_(disk,board)
            }
            put(board,disk,&p)
        };
        print!("{}",showBoard(&b));
        let p_= maxmin(&b,&nd);
        let b_= match p_ {
            Option::None => b,
            Option::Some(v) => put(&b,&nd,&v)
        };
        main_(disk,&b_)
    }
}

/*

main' disk board=do
  let nd=ndisk disk
  let p' =next board disk
  let p''=next board nd
  putStr $ showBoard board
  if p'==[] && p''==[] 
    then do
    if value board Black > value board White
      then print "black win"
      else if value board Black < value board White
        then print "white win"
        else print "draw"
    else do
    b <- case  p' of
      [] -> return board
      _  -> do
        v <- getLine
        let p=read v :: Pos
        if (filter (==p) p')==[] 
          then main' disk board
          else return ()
--        let (Just p)=maxmin board disk
        let b=put board disk p
        return b
    putStr $ showBoard b
    let p'=maxmin b nd 
    let b'=case p' of 
          Just v -> put b nd v
          _      -> b
    main' disk b'

initboard=M.insert (3,4) Black $
          M.insert (4,3) Black $
          M.insert (4,4) White $
          M.insert (3,3) White M.empty

main=do
  main' Black initboard

*/
fn main() {
    let mut initboard: Board = Board::new();
    initboard.insert((3,4),Disk::Black);
    initboard.insert((4,3),Disk::Black);
    initboard.insert((3,3),Disk::White);
    initboard.insert((4,4),Disk::White);
    main_(&Disk::Black,&initboard)
}

