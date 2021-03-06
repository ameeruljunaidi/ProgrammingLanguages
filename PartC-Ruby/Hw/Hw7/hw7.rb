# University of Washington, Programming Languages, Homework 7, hw7.rb
# (See also ML code)

# A little language for 2D geometry objects

# Each subclass of GeometryExpression, including subclasses of GeometryValue, needs to respond to messages
# preprocess_prog and eval_prog

# Each subclass of GeometryValue additionally needs:
#   - Shift
#   - Intersect, which uses the double-dispatch pattern
#   - IntersectPoint, intersectLine, and intersectVerticalLine for for being called by intersect of appropriate classes
#     and doing the correct intersection calculation
#   - (We would need intersectNoPoints and intersectLineSegment, but these are provided by GeometryValue and should not
#      be overridden.)
#   -  IntersectWithSegmentAsLineResult, which is used by intersectLineSegment as described in the assignment

# You can define other helper methods, but will not find much need to
# Note: geometry objects should be immutable: assign to fields only during object construction
# Note: For eval_prog, represent environments as arrays of 2-element arrays as described in the assignment

class GeometryExpression
  # Do *not* change this class definition
  Epsilon = 0.00001
end

class GeometryValue
  # Do *not* change methods in this class definition
  # You can add methods if you wish

  private

  # Some helper methods that may be generally useful
  def real_close(r1, r2)
    (r1 - r2).abs < GeometryExpression::Epsilon
  end

  def real_close_point(x1, y1, x2, y2)
    real_close(x1, x2) && real_close(y1, y2)
  end

  # Two_points_to_line could return a Line or a VerticalLine
  def two_points_to_line(x1, y1, x2, y2)
    if real_close(x1, x2)
      VerticalLine.new x1
    else
      m = (y2 - y1).to_f / (x2 - x1)
      b = y1 - m * x1
      Line.new(m, b)
    end
  end

  public

  # Note: no initialize method only because there is nothing it needs to do
  # ! This was moved from the NoPoints class to the superclass
  def preprocess_prog
    self # no pre-processing to do here
  end

  def eval_prog(_)
    self # all values evaluate to self
  end

  # We put this in this class so all subclasses can inherit it:
  # The intersection of self with a NoPoints is a NoPoints object
  def intersectNoPoints(np)
    NoPoints.new # Returns a new object to preserve immutability
  end

  # We put this in this class so all subclasses can inherit it:
  # The intersection of self with a LineSegment is computed by first intersecting with the line containing the segment
  # and then calling the result's intersectWithSegmentAsLineResult with the segment
  def intersectLineSegment(seg)
    line_result = intersect(two_points_to_line(seg.x1, seg.y1, seg.x2, seg.y2))
    line_result.intersectWithSegmentAsLineResult seg
  end
end

class NoPoints < GeometryValue
  # Do *not* change this class definition: everything is done for you (although this is the easiest class, it shows what
  # methods every subclass of geometry values needs) However, you *may* move methods from here to a superclass if you
  # wish to

  def shift(_, _)
    self # shifting no-points is no-points
  end

  def intersect(other)
    other.intersectNoPoints self # will be NoPoints but follow double-dispatch
  end

  def intersectPoint(_)
    self # intersection with point and no-points is no-points
  end

  def intersectLine(_)
    self # intersection with line and no-points is no-points
  end

  def intersectVerticalLine(_)
    self # intersection with line and no-points is no-points
  end

  # if self is the intersection of (1) some shape s and (2) the line containing seg, then we return the intersection of
  # the shape s and the seg.  seg is an instance of LineSegment
  def intersectWithSegmentAsLineResult(_)
    self
  end
end

class Point < GeometryValue
  # *Add* methods to this class -- do *not* change given code and do not override any methods

  # Note: You may want a private helper method like the local helper function between in the ML code
  attr_reader :x, :y

  def initialize(x, y)
    @x = x
    @y = y
  end

  def shift(dx, dy)
    Point.new(@x + dx, @y + dy)
  end

  def intersect(other)
    other.intersectPoint(self)
  end

  def intersectPoint(p)
    if real_close_point(@x, @y, p.x, p.y)
      self
    else
      NoPoints.new
    end
  end

  def intersectLine(line)
    if real_close(@y, line.m * @x + line.b)
      self
    else
      NoPoints.new
    end
  end

  def intersectVerticalLine(vline)
    if real_close(@x, vline.x)
      self
    else
      NoPoints.new
    end
  end

  def intersectWithSegmentAsLineResult(seg)
    if real_close(@x, seg.x1) or real_close(@x, seg.x2) or (@x > seg.x1 and @x < seg.x2)
      self
    else
      NoPoints.new
    end
  end
end

class Line < GeometryValue
  # *Add* methods to this class -- do *not* change given code and do not
  # override any methods
  attr_reader :m, :b

  def initialize(m, b)
    @m = m
    @b = b
  end

  def shift(dx, dy)
    Line.new(@m, @b + dy - @m * dx)
  end

  def intersect(other)
    other.intersectLine(self)
  end

  def intersectPoint(p)
    if real_close(p.y, @m * p.x + @b)
      Point.new(p.x, p.y)
    else
      NoPoints.new
    end
  end

  def intersectLine(line)
    if real_close(@m, line.m)
      if real_close(@b, line.b)
        self
      else
        NoPoints.new
      end
    else
      x = (line.b - @b) / (@m - line.m)
      y = @m * x + @b
      Point.new(x, y)
    end
  end

  def intersectVerticalLine(vline)
    Point.new(vline.x, @m * vline.x + @b)
  end

  def intersectWithSegmentAsLineResult(seg)
    seg
  end
end

class VerticalLine < GeometryValue
  # *Add* methods to this class -- do *not* change given code and do not override any methods
  attr_reader :x

  def initialize(x)
    @x = x
  end

  def shift(dx, _)
    VerticalLine.new(@x + dx)
  end

  def intersect(other)
    other.intersectVerticalLine(self)
  end

  def intersectPoint(p)
    if real_close(@x, p.x)
      Point.new(@x, p.y)
    else
      NoPoints.new
    end
  end

  def intersectLine(line)
    Point.new(@x, line.m * @x + line.b)
  end

  def intersectVerticalLine(vline)
    if real_close(@x, vline.x)
      self
    else
      NoPoints.new
    end
  end

  def intersectWithSegmentAsLineResult(seg)
    seg
  end
end

class LineSegment < GeometryValue
  # *Add* methods to this class -- do *not* change given code and do not override any methods
  # Note: This is the most difficult class. In the sample solution, preprocess_prog is about 15 lines long and
  # intersectWithSegmentAsLineResult is about 40 lines long
  attr_reader :x1, :y1, :x2, :y2

  def initialize(x1, y1, x2, y2)
    @x1 = x1
    @y1 = y1
    @x2 = x2
    @y2 = y2
  end

  # Override from super class
  def preprocess_prog
    if real_close_point(@x1, @y1, @x2, @y2)
      Point.new(@x1, @y1)
    elsif @x2 < @x1
      LineSegment.new(@x2, @y2, @x1, @y1)
    elsif real_close(@x1, @x2) and @y2 < @y1
      LineSegment.new(@x2, @y2, @x1, @y1)
    else
      self
    end
  end

  def shift(dx, dy)
    LineSegment.new(@x1 + dx, @y1 + dy, @x2 + dx, @y2 + dy)
  end

  def intersect(other)
    other.intersectLineSegment(self)
  end

  def intersectPoint(p)
    p.intersectLineSegment(self)
  end

  def intersectLine(line)
    line.intersectLineSegment(self)
  end

  def intersectVerticalLine(vline)
    vline.intersectLineSegment(self)
  end

  def intersectWithSegmentAsLineResult(seg)
    LineSegment.new([@x1, seg.x1].max, [@y1, seg.y1].max, [@x2, seg.x2].min, [@y2, seg.y2].min)
  end

end

# Note: there is no need for getter methods for the non-value classes

class Intersect < GeometryExpression
  # *Add* methods to this class -- do *not* change given code and do not override any methods
  def initialize(e1, e2)
    @e1 = e1
    @e2 = e2
  end

  def preprocess_prog
    Intersect.new(@e1.preprocess_prog, @e2.preprocess_prog)
  end

  def eval_prog(env)
    @e1.eval_prog(env).intersect(@e2.eval_prog(env))
  end

end

class Let < GeometryExpression
  # *Add* methods to this class -- do *not* change given code and do not override any methods
  # Note: Look at Var to guide how you implement Let
  def initialize(s, e1, e2)
    @s = s
    @e1 = e1
    @e2 = e2
  end

  def preprocess_prog
    # Pre-process the to expressions and return the same expression with the expressions evaluated
    Let.new(@s, @e1.preprocess_prog, @e2.preprocess_prog)
  end

  def eval_prog(env)
    # Evaluate e2 in an environment where e1 is evaluated and added to the environment
    @e2.eval_prog([[@s, @e1.eval_prog(env)]] + env)
  end
end

class Var < GeometryExpression
  # *Add* methods to this class -- do *not* change given code and do not override any methods
  def initialize(s)
    @s = s
  end

  def preprocess_prog
    self
  end

  def eval_prog(env)
    # Remember: do not change this method
    pr = env.assoc @s
    raise "undefined variable" if pr.nil?
    pr[1]
  end
end

class Shift < GeometryExpression
  # *Add* methods to this class -- do *not* change given code and do not override any methods
  def initialize(dx, dy, e)
    @dx = dx
    @dy = dy
    @e = e
  end

  def preprocess_prog
    Shift.new(@dx, @dy, @e.preprocess_prog)
  end

  def eval_prog(env)
    @e.eval_prog(env).shift(@dx, @dy)
  end
end
