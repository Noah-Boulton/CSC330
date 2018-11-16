# Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in,
# so do not modify the other files as
# part of your solution.

class MyPiece < Piece
    # The constant All_My_Pieces should be declared here:
    # class array holding all the pieces and their rotations

    All_My_Pieces = [[[[0, 0], [1, 0], [0, 1], [1, 1]]],  # square (only needs one)
                    rotations([[0, 0], [-1, 0], [1, 0], [0, -1]]), # T
                    [[[0, 0], [-1, 0], [1, 0], [2, 0]], # long (only needs two)
                    [[0, 0], [0, -1], [0, 1], [0, 2]]],
                    [[[0, 0], [-1, 0], [1, 0], [-2, 0], [2, 0]], # extra long (needs two)
                    [[0, 0], [0, -1], [0, 1], [0, -2], [0, 2]]],
                    rotations([[-1, 0], [0,0], [-1, -1]]), # extra small L 
                    rotations([[0, 0], [1, 0], [0, 1], [1, 1], [2, 1]]), # extra odd piece 
                    rotations([[0, 0], [0, -1], [0, 1], [1, 1]]), # L
                    rotations([[0, 0], [0, -1], [0, 1], [-1, 1]]), # inverted L
                    rotations([[0, 0], [-1, 0], [0, -1], [1, -1]]), # S
                    rotations([[0, 0], [1, 0], [0, -1], [-1, -1]])] # Z
    
                
    # Your Enhancements here
    def self.next_piece (board)
        if @cheat
            @cheat = false
            board.lower_score
            MyPiece.new([[[0,0]]], board)
        else
            MyPiece.new(All_My_Pieces.sample, board)
        end
    end

    def self.cheat ()
        @cheat = true
    end
    
end

class MyBoard < Board
    # Your Enhancements here:

    def initialize (game)
        @grid = Array.new(num_rows) {Array.new(num_columns)}
        @current_block = MyPiece.next_piece(self)
        @score = 0
        @game = game
        @delay = 500
    end

    # gets the next piece
    def next_piece
        @current_block = MyPiece.next_piece(self)
        @current_pos = nil
    end

    def lower_score()
        @score = @score - 100
        @game.update_score
    end

    # gets the information from the current piece about where it is and uses this
  # to store the piece on the board itself.  Then calls remove_filled.
  def store_current
    locations = @current_block.current_rotation
    displacement = @current_block.position
    (0..(locations.length - 1)).each{|index| 
      current = locations[index];
      @grid[current[1]+displacement[1]][current[0]+displacement[0]] = 
      @current_pos[index]
    }
    remove_filled
    @delay = [@delay - 2, 80].max
  end

end

class MyTetris < Tetris
    # Your Enhancements here
    def initialize
        super()
    end

    def set_board
        @canvas = TetrisCanvas.new
        @board = MyBoard.new(self)
        @canvas.place(@board.block_size * @board.num_rows + 3,
                      @board.block_size * @board.num_columns + 6, 24, 80)
        @board.draw
    end

    def key_bindings  
        super()
        @root.bind('u' , proc {@board.rotate_counter_clockwise; @board.rotate_counter_clockwise})
        @root.bind('c' , proc   {if @board.score > 99
                                    MyPiece.cheat()
                                end
                                })
    end

end


