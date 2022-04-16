# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

class MyPiece < Piece
  # The constant All_My_Pieces should be declared here

  All_My_Pieces = All_Pieces + [
    [
      [[0, -1], [0, -2], [0, 0], [0, 1], [0, 2]],
      [[2, 0], [1, 0], [0, 0], [-2, 0], [-1, 0]],
    ],
    rotations([[0, 0], [-1, 0], [-1, 1], [0, 1], [1, 0]]),
    rotations([[0, 0], [0, 1], [1, 0]]),
  ]

  # ! override self.next_piece to get All_My_Pieces
  def self.next_piece(board)
    MyPiece.new(All_My_Pieces.sample, board)
  end

  def self.cheat_piece(board)
    MyPiece.new([[0, 0]], board)
  end

  # your enhancements here

end

class MyBoard < Board
  # your enhancements here

  # ! override constructor to take from MyPiece
  def initialize(game)
    super
    @current_block = MyPiece.next_piece(self)
    @cheating = false
  end

  # ! override next_piece to use MyPiece
  def next_piece
    if @cheating
      @current_block = MyPiece.cheat_piece(self)
      @cheating = false
    else
      @current_block = MyPiece.next_piece(self)
    end
    @current_pos = nil
  end

  # ! override the loop and get rid of magic number 3
  # previous blocks only have 4 points, so the 3 was hardcoded
  # need to support any number of blocks since there are 3 and 5 blocks now
  def store_current
    locations = @current_block.current_rotation
    displacement = @current_block.position
    (0..locations.length - 1).each { |index|
      current = locations[index]
      @grid[current[1] + displacement[1]][current[0] + displacement[0]] = @current_pos[index]
    }
    remove_filled
    @delay = [@delay - 2, 80].max
  end

  # DONE: figure out if score is above cheat threshold
  # DONE: implement cheat process
  def cheat
    if @score > 100 and !@cheating
      @score -= 100
      @cheating = true
    end
  end

  # DONE: implement flip 180 degrees
  def flip
    if !game_over? and @game.is_running?
      @current_block.move(0, 0, 1)
      @current_block.move(0, 0, 1)
    end
    draw
  end
end

class MyTetris < Tetris
  # your enhancements here

  # ! override set_board to use MyBoard
  def set_board
    @canvas = TetrisCanvas.new
    @board = MyBoard.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end

  # DONE: implement 'u' to rotate in key_bindings
  # DONE: implement 'c' to cheat in key_bindings
  # ! override key_bindings to add new 'u' and 'c'
  def key_bindings
    super
    @root.bind("u", proc { @board.flip })
    @root.bind("c", proc { @board.cheat })
  end
end
