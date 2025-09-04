import { test, expect } from "@playwright/test";

test("add repository and verify on repositories page", async ({ page }) => {
  const repoName = `haskell-template-${Math.random().toString(36).substring(2, 8)}`;

  // Navigate to repositories page
  await page.goto("/");
  await page.getByRole("link", { name: /repositories/i }).click();

  // Add new repository
  await page.getByLabel("Repository Name").fill(repoName);
  await page
    .getByLabel("Git Clone URL")
    .fill("https://github.com/srid/haskell-template");
  await page.getByRole("button", { name: /add repository/i }).click();

  // Verify repository was added
  await page.reload();
  await expect(page.getByRole("link", { name: repoName })).toBeVisible();
});
