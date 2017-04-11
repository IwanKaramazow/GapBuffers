type t 'a = {size: int, buffer: Bytes.t, pre: int, post: int, gapCapacity: int};

/* TODO check size > gapCapacity */
let make ::gapCapacity=128 size => {
  size,
  buffer: Bytes.create size,
  pre: 0,
  post: gapCapacity,
  gapCapacity
};

let moveCursorForward ({pre, buffer, post, size} as gb) =>
  if (post === size - 1) {
    gb
  } else {
    Bytes.set buffer (pre + 1) (Bytes.get buffer post);
    {...gb, pre: gb.pre + 1, post: gb.post + 1}
  };

let moveCursorBackward ({pre, buffer, post} as gb) => {
  Bytes.set buffer (gb.post - 1) (Bytes.get buffer pre);
  {...gb, buffer, pre: pre - 1, post: post - 1}
};

let insertChar gb c => {
  let {pre, post, buffer, size} = gb;
  let gb =
    if (post === pre + 1) {
      let nextSize = size * 2;
      let newBuffer = Bytes.create nextSize;
      Bytes.blit buffer 0 newBuffer 0 pre;
      Bytes.blit buffer post newBuffer (post + gb.gapCapacity) (Bytes.length buffer);
      {...gb, buffer: newBuffer, post: post + gb.gapCapacity}
    } else {
      gb
    };
  let {pre, post, buffer, size} = gb;
  Bytes.set buffer pre c;
  {...gb, buffer, pre: pre + 1}
};
