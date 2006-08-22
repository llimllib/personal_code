import sys, pygame, time
pygame.init()

size = width, height = 320, 240
speed = [-1, -1]
black = 0, 0, 0

screen = pygame.display.set_mode(size)

ball = pygame.image.load("ball.bmp")
ballrect = ball.get_rect()

while 1:
    for event in pygame.event.get():
        if event.type == pygame.QUIT: sys.exit()

    ballrect = ballrect.move(speed)
    if ballrect.left < 0 or ballrect.right > width:
        speed[0] = -speed[0]
    if ballrect.top < 0 or ballrect.bottom > height:
        speed[1] = -speed[1]

    screen.fill(black)
    r = pygame.Rect((100, 100, 100, 100))
    rr = pygame.draw.rect(screen, (255,255,0), r, 0)
    pygame.display.update(r)
    time.sleep(1)
    screen.blit(ball, ballrect)
    pygame.display.flip()
