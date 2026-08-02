import pygame
from pygame.locals import *
from random import randint
from math import sin, cos, radians

# Initialize Pygame
pygame.init()

# Set up the display
WIDTH, HEIGHT = 800, 600
screen = pygame.display.set_mode((WIDTH, HEIGHT))
pygame.display.set_caption("Randomly Placed 3D Cubes")
clock = pygame.time.Clock()

# Define the colors
BLACK = (0, 0, 0)
WHITE = (255, 255, 255)
RED = (255, 0, 0)

# Cube size and grid dimensions
CUBE_SIZE = 50
GRID_SIZE = 10

# Generate random cube positions
cube_positions = []
for _ in range(10):
    x = randint(0, GRID_SIZE - 1) * CUBE_SIZE
    y = randint(0, GRID_SIZE - 1) * CUBE_SIZE
    z = randint(0, GRID_SIZE - 1) * CUBE_SIZE
    cube_positions.append((x, y, z))

# Camera variables
camera_distance = 500
camera_angle = 0


# Function to rotate a point around the origin
def rotate_point(point, angle):
    x, y, z = point
    rotated_x = x * cos(radians(angle)) - y * sin(radians(angle))
    rotated_y = x * sin(radians(angle)) + y * cos(radians(angle))
    return (rotated_x, rotated_y, z)


# Game loop
running = True
dragging = False
drag_start = None
while running:
    # Event handling
    for event in pygame.event.get():
        if event.type == QUIT:
            running = False
        elif event.type == MOUSEBUTTONDOWN:
            if event.button == 1:
                dragging = True
                drag_start = pygame.mouse.get_pos()
        elif event.type == MOUSEBUTTONUP:
            if event.button == 1:
                dragging = False
                drag_start = None

    # Clear the screen
    screen.fill(BLACK)

    # Render the grid
    for x in range(0, WIDTH, CUBE_SIZE):
        pygame.draw.line(screen, WHITE, (x, 0), (x, HEIGHT))
    for y in range(0, HEIGHT, CUBE_SIZE):
        pygame.draw.line(screen, WHITE, (0, y), (WIDTH, y))

    # Render the cubes
    for x, y, z in cube_positions:
        # Rotate the cube position based on camera angle
        rotated_x, rotated_y, _ = rotate_point((x, y, z), camera_angle)
        projected_x = rotated_x * camera_distance / (rotated_y + camera_distance)
        projected_y = rotated_y * camera_distance / (rotated_y + camera_distance)
        size = CUBE_SIZE - z

        # Draw the cube
        pygame.draw.rect(
            screen,
            WHITE,
            (projected_x + WIDTH / 2, projected_y + HEIGHT / 2, size, size),
        )

    # Update the display
    pygame.display.flip()
    clock.tick(60)

    # Update camera rotation if dragging
    if dragging:
        mouse_pos = pygame.mouse.get_pos()
        drag_distance = mouse_pos[0] - drag_start[0]
        camera_angle += drag_distance * 0.5

# Quit the game
pygame.quit()
